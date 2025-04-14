(* TODO: we need to declare the types of the stack slots *)
open Std
open Ae_abs_x86_types
module Temp_entity = Ae_abs_asm_temp_entity
module Temp = Temp_entity.Ident
module Entity = Ae_entity_std
module Ident = Entity.Ident
module Label = Ae_label_entity.Ident
module Stack_slot = Ae_stack_slot_entity.Ident
module Table = Ident.Table
module Id_gen = Entity.Id_gen
open Table.Syntax
open Ae_trace

let init_usual ~num_regs ~preds ~next_use_in ~active_out_table =
  let preds_num = List.length preds in
  let temp_freq = Table.create () in
  let in_all_preds = ref Ident.Set.empty in
  let in_some_pred = ref Ident.Set.empty in
  begin
    let@: pred = List.iter preds in
    begin
      let@: temp =
        (* pred might not have been computed yet, like in loops *)
        Table.find active_out_table pred
        |> Option.value ~default:Ident.Set.empty
        |> Ident.Set.iter
        (* TODO: add this back *)
        (* |> Iter.filter ~f:(Ident.Map.mem next_use_in) *)
      in
      temp_freq.!(temp) <- Table.find_or_add temp_freq temp ~default:(Fn.const 0) + 1;
      in_some_pred := Ident.Set.add !in_some_pred temp;
      if temp_freq.!(temp) = preds_num
      then begin
        in_some_pred := Ident.Set.remove !in_some_pred temp;
        in_all_preds := Ident.Set.add !in_all_preds temp
      end
    end
  end;
  let in_all = !in_all_preds in
  let in_some = !in_some_pred in
  let temps_to_add =
    Ident.Set.to_list in_some
    |> List.take __ (Ident.Set.length in_all - num_regs)
    |> Ident.Set.of_list_exn
  in
  Ident.Set.union in_all temps_to_add
;;

let sort_temps_by_ascending_next_use next_use temps =
  List.map temps ~f:(fun temp ->
    temp, Ident.Map.find next_use temp |> Option.value ~default:Int.max_value)
  |> List.sort ~compare:(Fn.on snd compare)
  |> List.map ~f:fst
;;

(* Make sure to set the active in table after running spill_block *)
let spill_block ~num_regs ~active_in ~next_use_out ~spill_temp ~edit (block : Block.t) =
  let next_uses_at_point =
    let next_use = ref next_use_out in
    let next_uses = Array.create ~len:(Arrayp.length block.body + 1) !next_use in
    Arrayp.iteri_rev block.body ~f:(fun i instr ->
      next_uses.(i) <- !next_use;
      next_use := Liveness.next_use_backwards_transfer instr.i !next_use);
    next_uses
  in
  let active = ref active_in in
  begin
    let@: instr' = Block.iter_fwd block in
    let _next_uses_here_in = next_uses_at_point.(instr'.index) in
    let next_uses_here_out = next_uses_at_point.(instr'.index + 1) in
    begin
      match instr'.i with
      | Block_params params ->
        let temps =
          List.filter_map params ~f:(fun param ->
            Location.temp_val param.Block_param.param)
          |> sort_temps_by_ascending_next_use next_uses_here_out
        in
        let temps_in_reg, temps_spilled =
          List.split_n temps (num_regs - Ident.Set.length !active)
        in
        let temps_spilled =
          List.map temps_spilled ~f:(fun temp -> temp, spill_temp temp)
          |> Ident.Map.of_alist_exn
        in
        let new_params =
          (List.map & Traverse.of_field Block_param.Fields.param) params ~f:(function
            | Slot slot -> Location.Slot slot
            | Temp temp as loc ->
              Ident.Map.find temps_spilled temp
              |> Option.value_map ~f:(Fn.compose Location.slot fst) ~default:loc)
        in
        Multi_edit.add_replace
          edit
          block.label
          { instr' with i = Block_params new_params };
        let non_dead_temps_in_reg =
          List.filter temps_in_reg ~f:(fun temp -> Ident.Map.mem next_uses_here_out temp)
        in
        active := List.fold_left non_dead_temps_in_reg ~init:!active ~f:Ident.Set.add;
        ()
      | instr when Instr.is_control instr ->
        (* for control instructions, set each non active temp to be a slot in the block_call *)
        let instr =
          begin
            let@: block_call = Instr.map_block_calls instr in
            let non_active_uses =
              Instr.iter_uses instr
              |> Iter.filter ~f:(fun temp -> not (Ident.Set.mem !active temp))
              |> Iter.to_list
            in
            let temps_spilled =
              List.map non_active_uses ~f:(fun temp -> temp, spill_temp temp)
              |> Ident.Map.of_alist_exn
            in
            let new_args =
              List.map block_call.args ~f:(function
                | Slot slot -> Location.Slot slot
                | Temp temp as loc ->
                  Ident.Map.find temps_spilled temp
                  |> Option.value_map ~f:(Fn.compose Location.slot fst) ~default:loc)
            in
            { block_call with args = new_args }
          end
        in
        Multi_edit.add_replace edit block.label { instr' with i = instr };
        ()
      | instr ->
        let uses = Instr.iter_uses instr |> Iter.to_list in
        assert (num_regs >= List.length uses);
        let non_active_uses =
          uses |> List.filter ~f:(fun temp -> not (Ident.Set.mem !active temp))
        in
        let defs = Instr.iter_defs instr |> Iter.to_list in
        (* insert reloads for the non_active_uses *)
        begin
          let@: temp = List.iter non_active_uses in
          let stack_slot, ty = spill_temp temp in
          let reload_instr =
            Instr.Mov { dst = Reg temp; src = Stack_slot stack_slot; size = ty }
          in
          Multi_edit.add_insert
            edit
            block.label
            (Instr'.create
               ?info:(Option.map ~f:(Info.tag_s ~tag:[%message "reload"]) instr'.info)
               reload_instr
               instr'.index)
        end;
        let num_clobbers = Instr.iter_clobbers instr |> Iter.length in
        assert (num_regs >= List.length defs + num_clobbers);
        let new_active =
          Ident.Set.to_list !active
          |> sort_temps_by_ascending_next_use next_uses_here_out
          |> List.append __ non_active_uses
          |> List.append __ defs
        in
        let num_active = List.length new_active + num_clobbers in
        active := List.drop new_active (num_active - num_regs) |> Ident.Set.of_list_exn
    end
  end;
  !active
;;

let fixup_edge
      ~edit
      ~src_active_out
      ~dst_active_in
      ~src_num_successors
      ~src_block
      ~dst_block
      ~spill_temp
  =
  let need_to_reload =
    Ident.Set.fold src_active_out ~init:dst_active_in ~f:Ident.Set.remove
    |> Ident.Set.to_list
  in
  if not (List.is_empty need_to_reload)
  then begin
  end;
  let reload_instrs =
    List.map need_to_reload ~f:(fun temp ->
      let stack_slot, ty = spill_temp temp in
      let instr = Instr.Mov { dst = Reg temp; src = Stack_slot stack_slot; size = ty } in
      instr)
  in
  if src_num_successors = 1
  then begin
    let control_instr = Block.find_control src_block in
    (* insert the code before the control instr at the source block *)
    let instrs = List.map reload_instrs ~f:(Instr'.create __ control_instr.index) in
    Multi_edit.add_inserts edit src_block.label instrs
  end
  else begin
    let block_params = Block.find_block_params dst_block in
    (* insert after block params in destination block *)
    let instrs = List.map reload_instrs ~f:(Instr'.create __ (block_params.index + 1)) in
    Multi_edit.add_inserts edit dst_block.label instrs
  end
;;

let fixup_func ~active_in_table ~active_out_table ~spill_temp func =
  let edit = Multi_edit.create () in
  let succ_table = Func.succ_table func in
  begin
    let@: block = Func.iter_blocks func in
    let control_instr = Block.find_control block in
    let@: block_call = Instr.iter_block_calls control_instr.i in
    (* every block should be computed, so table lookup can't panic *)
    let src_active_out = active_out_table.!(block.label) in
    let dst_active_in = active_in_table.!(block_call.label) in
    let src_num_successors = List.length succ_table.!(block.label) in
    fixup_edge
      ~edit
      ~src_active_out
      ~dst_active_in
      ~src_num_successors
      ~src_block:block
      ~dst_block:(Func.find_block_exn func block_call.label)
      ~spill_temp
  end;
  Func.apply_multi_edit edit func
;;

(*
   enhancement: we want to find the optimal place to spill the variable
  This is the place where the variable is still in a register and dominates all reloads,
*)
let insert_spills ~spilled func =
  let edit = Multi_edit.create () in
  let labels = Func.labels_postorder func in
  (*
     Since we call insert_spills after we add a bunch of reloads, we are not in ssa form anymore
    This means we have multiple definitions of one variable.
    We don't want to spill a variable multiple times.
    However, we are guaranteed to hit the original first definition if we traverse in dominator order.
  *)
  let already_spilled = Table.create () in
  begin
    let@: label = Vec.iter_rev labels in
    let block = Func.find_block_exn func label in
    let@: instr = Block.iter_fwd block in
    let@: def, (stack_slot, ty) =
      Instr.iter_defs instr.i
      |> Iter.filter_map ~f:(fun def ->
        Hashtbl.find spilled def |> Option.map ~f:(Tuple2.create def))
      (* |> Iter.filter ~f:(fun (def, _) -> not (Table.mem already_spilled def)) *)
    in
    already_spilled.!(def) <- ();
    let new_instr = Instr.Mov { dst = Stack_slot stack_slot; src = Reg def; size = ty } in
    Multi_edit.add_insert edit block.label (Instr'.create new_instr (instr.index + 1))
  end;
  Func.apply_multi_edit edit func
;;

(* critical edges MUST be split before calling this function, because we have to insert fixup code on the edges *)
let spill_func ~num_regs (func : Func.t) =
  let ty_table = Func.get_ty_table func in
  let spilled = Hashtbl.create (module Temp) in
  let stack_builder = Func.create_stack_builder func in
  let spill_temp temp =
    Hashtbl.find_or_add spilled temp ~default:(fun () ->
      let ty = ty_table.!(temp) in
      let stack_slot =
        Stack_builder.alloc stack_builder ~name:("pre_spilled_" ^ temp.name) ty
      in
      stack_slot, ty)
  in
  let pred_table = Func.pred_table func in
  let next_use_in_table, next_use_out_table =
    Liveness.compute_next_use_distance ~pred_table func
  in
  let labels_order = Func.labels_reverse_postorder func in
  let active_in_table = Table.create () in
  let active_out_table = Table.create () in
  let func =
    let edit = Multi_edit.create () in
    begin
      let@: label = Vec.iter labels_order in
      let block = Func.find_block_exn func label in
      (* TODO: for active_in we need to remove variables that were dead *)
      let active_in =
        init_usual
          ~num_regs
          ~preds:pred_table.!(label)
          ~next_use_in:(Liveness.Next_use_table.find next_use_in_table label)
          ~active_out_table
      in
      let next_use_out = Liveness.Next_use_table.find next_use_out_table label in
      let active_out =
        spill_block ~num_regs ~active_in ~next_use_out ~spill_temp ~edit block
      in
      active_in_table.!(label) <- active_in;
      active_out_table.!(label) <- active_out
    end;
    Func.apply_multi_edit edit func
  in
  let func = fixup_func ~active_in_table ~active_out_table ~spill_temp func in
  let func = Func.apply_stack_builder stack_builder func in
  let func = insert_spills ~spilled func in
  (* TODO: we should do this after we apply the multi edit, or else we are inserting multiple spills *)
  let func = Convert_ssa.convert func in
  func
;;
