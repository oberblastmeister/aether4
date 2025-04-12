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

let init_usual ~num_regs ~preds ~active_out_table =
  let preds_num = List.length preds in
  let temp_freq = Table.create () in
  let in_all = ref Ident.Set.empty in
  let in_some = ref Ident.Set.empty in
  begin
    let@: pred = List.iter preds in
    begin
      let@: temp = Ident.Set.iter active_out_table.!(pred) in
      temp_freq.!(temp) <- temp_freq.!(temp) + 1;
      in_some := Ident.Set.add !in_some temp;
      if temp_freq.!(temp) = preds_num
      then begin
        in_some := Ident.Set.remove !in_some temp;
        in_all := Ident.Set.add !in_all temp
      end
    end
  end;
  let in_all = !in_all in
  let in_some = !in_some in
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
let spill_block
      ~num_regs
      (* ~active_out_table *)
      ~active_in
      (* ~pred_table *)
      ~next_use_out
      ~spill_temp
      ~edit
      (block : Block.t)
  =
  let next_uses_at_point =
    let next_use = ref next_use_out in
    Arrayp.mapi_rev block.body ~f:(fun _ instr ->
      next_use := Liveness.next_use_backwards_transfer instr.i !next_use;
      !next_use)
  in
  let active = ref active_in in
  begin
    let@: instr' = Block.iter_fwd block in
    let next_uses_here = next_uses_at_point.@(instr'.index) in
    begin
      match instr'.i with
      | Block_params params ->
        let temps =
          List.filter_map params ~f:(fun param ->
            Location.temp_val param.Block_param.param)
          |> sort_temps_by_ascending_next_use next_uses_here
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
              |> Option.value_map ~f:Location.slot ~default:loc)
        in
        Multi_edit.add_insert edit block.label { instr' with i = Block_params new_params };
        let non_dead_temps_in_reg =
          List.filter temps_in_reg ~f:(fun temp -> Ident.Map.mem next_uses_here temp)
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
              |> sort_temps_by_ascending_next_use next_uses_here
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
                  |> Option.value_map ~f:Location.slot ~default:loc)
            in
            { block_call with args = new_args }
          end
        in
        Multi_edit.add_insert edit block.label { instr' with i = instr };
        ()
      | instr ->
        let non_active_uses =
          Instr.iter_uses instr
          |> Iter.filter ~f:(fun temp -> not (Ident.Set.mem !active temp))
          |> Iter.to_list
        in
        let defs = Instr.iter_defs instr |> Iter.to_list in
        (* insert reloads for the non_active_uses *)
        begin
          let@: temp = List.iter non_active_uses in
          let stack_slot = spill_temp temp in
          let reload_instr =
            Instr.Mov { dst = Reg temp; src = Stack_slot stack_slot; size = Qword }
          in
          Multi_edit.add_insert
            edit
            block.label
            (Instr'.create
               ~info:(Info.create_s [%message "reload"])
               reload_instr
               instr'.index)
        end;
        let num_clobbers = Instr.iter_clobbers instr |> Iter.length in
        assert (num_regs > List.length non_active_uses + List.length defs + num_clobbers);
        let sorted_active =
          Ident.Set.to_list !active
          |> sort_temps_by_ascending_next_use next_uses_here
          |> List.append __ non_active_uses
          |> List.append __ defs
        in
        let num_active = List.length sorted_active + num_clobbers in
        let new_active_list = List.drop sorted_active (num_active - num_regs) in
        active := Ident.Set.of_list_exn new_active_list
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
  let reload_instrs =
    List.map need_to_reload ~f:(fun temp ->
      let stack_slot = spill_temp temp in
      let instr =
        Instr.Mov { dst = Reg temp; src = Stack_slot stack_slot; size = Qword }
      in
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

let fixup_func ~edit ~active_in_table ~active_out_table ~spill_temp func =
  let succ_table = Func.succ_table func in
  begin
    let@: block = Func.iter_blocks func in
    let control_instr = Block.find_control block in
    let@: block_call = Instr.iter_block_calls control_instr.i in
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
  end
;;

let insert_spills ~spilled func =
  let edit = Multi_edit.create () in
  begin
    let@: block = Func.iter_blocks func in
    let@: instr = Block.iter_fwd block in
    let@: def, stack_slot =
      Instr.iter_defs instr.i
      |> Iter.filter_map ~f:(fun def ->
        Hashtbl.find spilled def |> Option.map ~f:(Tuple2.create def))
    in
    let new_instr =
      Instr.Mov { dst = Stack_slot stack_slot; src = Reg def; size = Qword }
    in
    Multi_edit.add_insert edit block.label (Instr'.create new_instr (instr.index + 1))
  end;
  Func.apply_multi_edit ~no_sort:() edit func
;;

(* critical edges MUST be split before calling this function, because we have to insert fixup code on the edges *)
let spill_func ~num_regs (func : Func.t) =
  let spilled = Hashtbl.create (module Temp) in
  let func =
    let pred_table = Func.pred_table func in
    let _next_use_in_table, next_use_out_table =
      Liveness.compute_next_use_distance ~pred_table func
    in
    let labels_order = Func.labels_reverse_postorder func in
    let stack_builder = Func.create_stack_builder func in
    let spill_temp temp =
      Hashtbl.find_or_add spilled temp ~default:(fun () ->
        Stack_builder.alloc stack_builder ~name:("pre_spilled" ^ temp.name) Qword)
    in
    let active_in_table = Table.create () in
    let active_out_table = Table.create () in
    let edit = Multi_edit.create () in
    begin
      let@: label = Vec.iter labels_order in
      let block = Func.find_block_exn func label in
      let active_in = init_usual ~num_regs ~preds:pred_table.!(label) ~active_out_table in
      let next_use_out = next_use_out_table.!(label) in
      let active_out =
        spill_block ~num_regs ~active_in ~next_use_out ~spill_temp ~edit block
      in
      active_in_table.!(label) <- active_in;
      active_out_table.!(label) <- active_out
    end;
    fixup_func ~edit ~active_in_table ~active_out_table ~spill_temp func;
    Func.apply_multi_edit edit func |> Func.apply_stack_builder stack_builder
  in
  let func = insert_spills ~spilled func in
  let func = Convert_ssa.convert func in
  func
;;
