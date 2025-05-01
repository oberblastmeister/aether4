(* TODO: add stack allocator to share stack slots better *)
(*
  TODO: optimization, for instructions that can take stack slots as arguments,
  we currently just add all inactive arguments as stack slots.
  We should try to reload as many inactive arguments as we can,
  and the put the remaining arguments as stack slots.
*)
(* 
  preconditions for instructions before pre_spill is
  
  forall non ssa instructions:
  |uses| <= num_regs
  |defs| <= num_regs
  
  pre_spill should satisfy the postcondition.
  
  forall non ssa instructions,
  |defs| + |clobbers| + |live_through| <= num_regs
*)
open Std
open Ae_abs_x86_types
module Label = Ae_label
module Temp = Ae_temp
open Ae_trace

module Spiller : sig
  type t [@@deriving sexp_of]

  val create : Func.t -> t
  val spill : ?is_block_param:unit -> t -> Temp.t -> Stack_address.t * Ty.t
  val apply_func : t -> Func.t -> Func.t
  val stack_slots : t -> (Stack_slot.t * Ty.t) list
  val find : t -> Temp.t -> (Stack_address.t * Ty.t) option

  (* was this a block param and was also spilled? *)
  val did_spill_block_param : t -> Temp.t -> bool
end = struct
  type t =
    { spilled : (Temp.t, Stack_address.t * Ty.t) Hashtbl.t
    ; spilled_block_params : Temp.t Hash_set.t
    ; stack_builder : Stack_builder.t
    ; ty_table : Ty.t Temp.Table.t
    }
  [@@deriving sexp_of]

  let find t temp = Hashtbl.find t.spilled temp

  let create func =
    let spilled = Hashtbl.create (module Temp) in
    let ty_table = Func.get_ty_table func in
    let spilled_block_params = Hash_set.create (module Temp) in
    begin
      let@: block = Func.iter_blocks func in
      let is_start = Label.equal block.label func.start in
      let@: instr' = Block.iter_fwd block in
      let instr = instr'.i in
      if is_start && instr'.index = 0
      then begin
        let locations_on_stack =
          Instr.block_params_val instr
          |> Option.value_exn
          |> List.map ~f:Block_param.param
          |> List.drop __ func.call_conv.num_args_in_regs
        in
        begin
          let@: i, loc = List.iteri locations_on_stack |> Iter.uncurry in
          let@: temp = Location.temp_val loc |> Option.iter in
          Hash_set.add spilled_block_params temp;
          Hashtbl.add_exn
            spilled
            ~key:temp
            ~data:
              ( Stack_address.Previous_frame Int32.(of_int_exn i * 8l)
              , ty_table.Temp.!(temp) )
        end
      end
    end;
    { ty_table
    ; spilled
    ; spilled_block_params
    ; stack_builder = Func.create_stack_builder func
    }
  ;;

  let spill ?is_block_param t temp =
    Hashtbl.find_or_add t.spilled temp ~default:(fun () ->
      if Option.is_some is_block_param then Hash_set.add t.spilled_block_params temp;
      let ty = t.ty_table.Temp.!(temp) in
      let stack_slot =
        Stack_builder.alloc t.stack_builder ~name:("pre_spilled_" ^ temp.name) ty
      in
      Stack_address.Slot stack_slot, ty)
  ;;

  let apply_func t func = Func.apply_stack_builder t.stack_builder func
  let stack_slots t = t.stack_builder.stack_slots
  let did_spill_block_param t temp = Hash_set.mem t.spilled_block_params temp
end

let sort_temps_by_ascending_next_use next_use temps =
  List.map temps ~f:(fun temp ->
    temp, Map.find next_use temp |> Option.value ~default:Int.max_value)
  |> List.sort ~compare:(Fn.on snd compare)
  |> List.map ~f:fst
;;

let init_usual ~num_regs ~preds ~next_use_in ~active_out_table =
  let preds_num = List.length preds in
  let temp_freq = Temp.Table.create () in
  let in_all_preds = ref Temp.Set.empty in
  let in_some_pred = ref Temp.Set.empty in
  begin
    let@: pred = List.iter preds in
    begin
      let@: temp =
        (* pred might not have been computed yet, like in loops *)
        Label.Table.find active_out_table pred
        |> Option.value ~default:Temp.Set.empty
        |> Set.iter
        (* removing this somehow breaks the ssa conversion *)
        |> Iter.filter ~f:(Map.mem next_use_in)
      in
      temp_freq.Temp.!(temp)
      <- Temp.Table.find_or_add temp_freq temp ~default:(Fn.const 0) + 1;
      in_some_pred := Set.add !in_some_pred temp;
      if temp_freq.Temp.!(temp) = preds_num
      then begin
        in_some_pred := Set.remove !in_some_pred temp;
        in_all_preds := Set.add !in_all_preds temp
      end
    end
  end;
  let in_all_preds =
    Set.to_list !in_all_preds |> sort_temps_by_ascending_next_use next_use_in
  in
  let in_some_pred =
    Set.to_list !in_some_pred |> sort_temps_by_ascending_next_use next_use_in
  in
  in_all_preds |> List.append __ in_some_pred |> List.take __ num_regs |> Temp.Set.of_list
;;

(*
   We make sure that if adding the non active temporaries to the active list
would exceed the number of registers that we have, then we evict some registers from the active list
*)
let add_to_active_list
      ?(num_virtual_non_active = 0)
      ~num_regs
      ~active_list
      ~next_uses
      non_active
  =
  sort_temps_by_ascending_next_use next_uses active_list
  |> List.take __ (num_regs - (List.length non_active + num_virtual_non_active))
  |> List.append __ non_active
;;

(* Make sure to set the active in table after running spill_block *)
let spill_block ~num_regs ~active_in ~next_use_out ~spiller ~edit (block : Block.t) =
  let next_uses_at_point =
    let next_use = ref next_use_out in
    let next_uses = Array.create ~len:(Arrayp.length block.body + 1) !next_use in
    Arrayp.iteri_rev block.body ~f:(fun i instr ->
      next_use := Liveness.next_use_backwards_transfer instr.i !next_use;
      next_uses.(i) <- !next_use);
    next_uses
  in
  let active = ref active_in in
  begin
    let@: instr' = Block.iter_fwd block in
    (*
      if something is not in this map it means it is dead.
      We can also think of it as having next use distance infinity
    *)
    (* TODO: there is something wrong with the next_uses_at_point implementation *)
    let next_uses_here_in = next_uses_at_point.(instr'.index) in
    let next_uses_here_out = next_uses_at_point.(instr'.index + 1) in
    begin
      match instr'.i with
      | Block_params params ->
        (* TODO: spill these in the proper places *)
        let temps =
          List.filter_map params ~f:(fun param ->
            Location.temp_val param.Block_param.param)
          |> sort_temps_by_ascending_next_use next_uses_here_out
        in
        let temps_in_reg, temps_spilled =
          List.split_n temps (num_regs - Set.length !active)
        in
        let temps_spilled =
          List.map temps_spilled ~f:(fun temp ->
            temp, Spiller.spill ~is_block_param:() spiller temp)
          |> Temp.Map.of_alist_exn
        in
        let new_params =
          (List.map & Traverse.of_field Block_param.Fields.param) params ~f:(function
            | Stack slot -> Location.Stack slot
            | Temp temp as loc ->
              Map.find temps_spilled temp
              |> Option.value_map ~f:(fun (slot, _) -> Stack slot) ~default:loc)
        in
        Multi_edit.add_replace
          edit
          block.label
          { instr' with i = Block_params new_params };
        let non_dead_temps_in_reg =
          List.filter temps_in_reg ~f:(fun temp -> Map.mem next_uses_here_out temp)
        in
        active := List.fold_left non_dead_temps_in_reg ~init:!active ~f:Set.add;
        ()
      | instr when Instr.is_call instr ->
        let clobbers = Instr.iter_clobbers instr |> Iter.to_list in
        let non_active_uses =
          Instr.iter_uses instr
          |> Iter.filter ~f:(fun temp -> not (Set.mem !active temp))
          |> Iter.to_list
        in
        let temps_spilled =
          List.map non_active_uses ~f:(fun temp -> temp, Spiller.spill spiller temp)
          |> Temp.Map.of_alist_exn
        in
        let `dsts dsts, `func func, `args args, `call_conv call_conv =
          Instr.call_val instr |> Option.value_exn
        in
        let new_args =
          (List.map & Tuple2.map_fst) args ~f:(function
            | Stack slot -> Location.Stack slot
            | Temp temp as loc ->
              Map.find temps_spilled temp
              |> Option.value_map ~f:(fun (slot, _) -> Stack slot) ~default:loc)
        in
        let new_instr = Instr.Call { dsts; func; args = new_args; call_conv } in
        Multi_edit.add_replace edit block.label { instr' with i = new_instr };
        let active_list = Set.to_list !active in
        let active_list =
          add_to_active_list
            ~num_regs
            ~active_list
            ~next_uses:next_uses_here_out
            ~num_virtual_non_active:(List.length clobbers)
            (List.map ~f:fst dsts)
        in
        active := Temp.Set.of_list active_list
      | instr when Instr.is_control instr ->
        (* for control instructions, set each non active temp to be a slot in the block_call *)
        let instr =
          begin
            let@: block_call = Instr.map_block_calls instr in
            let non_active_uses =
              Instr.iter_uses instr
              |> Iter.filter ~f:(fun temp -> not (Set.mem !active temp))
              |> Iter.to_list
            in
            let temps_spilled =
              List.map non_active_uses ~f:(fun temp -> temp, Spiller.spill spiller temp)
              |> Temp.Map.of_alist_exn
            in
            let new_args =
              List.map block_call.args ~f:(function
                | Stack slot -> Location.Stack slot
                | Temp temp as loc ->
                  Map.find temps_spilled temp
                  |> Option.value_map ~f:(fun (slot, _) -> Stack slot) ~default:loc)
            in
            { block_call with args = new_args }
          end
        in
        Multi_edit.add_replace edit block.label { instr' with i = instr };
        ()
      | instr ->
        let uses = Instr.iter_uses instr |> Iter.to_list in
        (* TODO: this fails because we need to make Call take a location, and then spill it independently just like a block_call *)
        assert (List.length uses <= num_regs);
        let non_active_uses =
          uses |> List.filter ~f:(fun temp -> not (Set.mem !active temp))
        in
        let defs = Instr.iter_defs instr |> Iter.to_list in
        (* make space for the uses *)
        let active_list =
          add_to_active_list
            ~num_regs
            ~active_list:(Set.to_list !active)
            ~next_uses:next_uses_here_in
            non_active_uses
        in
        (* insert reloads for the non_active_uses *)
        begin
          let@: temp = List.iter non_active_uses in
          let stack, ty = Spiller.spill spiller temp in
          let reload_instr = Instr.Mov { dst = Reg temp; src = Stack stack; size = ty } in
          Multi_edit.add_insert
            edit
            block.label
            (Instr'.create
               ?info:(Option.map ~f:(Info.tag_s ~tag:[%message "reload"]) instr'.info)
               reload_instr
               instr'.index)
        end;
        (* make spaces for the definitions, including clobbers *)
        let num_clobbers = Instr.iter_clobbers instr |> Iter.length in
        assert (List.length defs + num_clobbers <= num_regs);
        let active_list =
          add_to_active_list
            ~num_regs
            ~active_list
              (* it is important that we use next_uses_here_out so that dead uses get removed first *)
            ~next_uses:next_uses_here_out
            ~num_virtual_non_active:num_clobbers
            defs
        in
        active := Temp.Set.of_list active_list
    end
  end;
  !active
;;

let fixup_edge
      ~edit
      ~src_active_out
      ~dst_active_in
      ~src_block
      ~(dst_block : Block.t)
      ~spiller
      ~dst_block_total_params
  =
  let need_to_reload temp =
    Set.mem dst_active_in temp && not (Set.mem src_active_out temp)
  in
  begin
    let new_args =
      List.map dst_block_total_params ~f:(fun temp ->
        if need_to_reload temp
        then Spiller.spill spiller temp |> fst |> Location.Stack
        else Location.Temp temp)
    in
    let control_instr = Block.find_control src_block in
    let control_instr =
      let@: instr = Instr'.map control_instr in
      let@: block_call =
        (Instr.map_block_calls
         & Traverse.filtered ~f:(fun (block_call : Block_call.t) ->
           Label.equal dst_block.label block_call.label))
          instr
      in
      { block_call with args = block_call.args @ new_args }
    in
    Multi_edit.add_replace edit src_block.label control_instr
  end
;;

let calculate_total_params_for_block
      ~pred_table
      ~active_in_table
      ~active_out_table
      (block : Block.t)
  =
  let total_params = ref Temp.Set.empty in
  begin
    let@: pred = List.iter pred_table.Label.!(block.label) in
    let src_active_out = active_out_table.Label.!(pred) in
    let dst_active_in = active_in_table.Label.!(block.label) in
    let need_to_reload = Set.fold ~init:dst_active_in ~f:Set.remove src_active_out in
    total_params := Set.union !total_params need_to_reload
  end;
  Set.to_list !total_params
;;

let fixup_func ~ty_table ~pred_table ~active_in_table ~active_out_table ~spiller func =
  let edit = Multi_edit.create () in
  begin
    let@: dst_block = Func.iter_blocks func in
    let dst_block_total_params =
      calculate_total_params_for_block
        ~pred_table
        ~active_in_table
        ~active_out_table
        dst_block
    in
    let dst_block_total_params_with_ty =
      List.map dst_block_total_params ~f:(fun temp ->
        { Block_param.param = Location.Temp temp; ty = ty_table.Temp.!(temp) })
    in
    let dst_active_in = active_in_table.Label.!(dst_block.label) in
    begin
      let@: pred = List.iter pred_table.Label.!(dst_block.label) in
      let src_block = Func.find_block_exn func pred in
      (* every block should be computed, so table lookup can't panic *)
      let src_active_out = active_out_table.Label.!(src_block.label) in
      fixup_edge
        ~edit
        ~src_active_out
        ~dst_active_in
        ~src_block
        ~dst_block
        ~spiller
        ~dst_block_total_params
    end;
    let block_params = Block.find_block_params dst_block in
    let block_params =
      let@: block_params = Instr'.map block_params in
      let params = Instr.block_params_val block_params |> Option.value_exn in
      Instr.block_params (params @ dst_block_total_params_with_ty)
    in
    Multi_edit.add_replace edit dst_block.label block_params
  end;
  Func.apply_multi_edit edit func
;;

(*
  enhancement: we want to find the optimal place to spill the variable
  This is the place where the variable is still in a register and dominates all reloads, but is in the least nested loop.
*)
(* TODO: don't insert spills for BlockParams *)
let insert_spills ~spiller func =
  let edit = Multi_edit.create () in
  let labels = Func.labels_postorder func in
  (*
     Since we call insert_spills after we add a bunch of reloads, which redefine the variables, we are not in ssa form anymore
    This means we have multiple definitions of one variable.
    We don't want to spill a variable multiple times.
    However, we are guaranteed to hit the original first definition if we traverse in dominator order.
  *)
  let already_spilled = Temp.Table.create () in
  begin
    let@: label = Vec.iter_rev labels in
    let block = Func.find_block_exn func label in
    let@: instr = Block.iter_fwd block in
    let@: def, (stack_slot, ty) =
      Instr.iter_defs instr.i
      (* we skip block param defs because those were already spilled directly by replacing the def temp with a stack slot in the block param *)
      |> Iter.filter ~f:(fun def -> not (Spiller.did_spill_block_param spiller def))
      |> Iter.filter_map ~f:(fun def ->
        Spiller.find spiller def |> Option.map ~f:(Tuple2.create def))
      |> Iter.filter ~f:(fun (def, _) -> not (Temp.Table.mem already_spilled def))
    in
    already_spilled.Temp.!(def) <- ();
    let new_instr = Instr.Mov { dst = Stack stack_slot; src = Reg def; size = ty } in
    Multi_edit.add_insert
      edit
      block.label
      (Instr'.create ~info:(Info.create_s [%message "spill"]) new_instr (instr.index + 1))
  end;
  Func.apply_multi_edit edit func
;;

let spill_func ~num_regs (func : Func.t) =
  let ty_table = Func.get_ty_table func in
  let spiller = Spiller.create func in
  let pred_table = Func.pred_table func in
  let next_use_in_table, next_use_out_table =
    Liveness.compute_next_use_distance ~pred_table func
  in
  let labels_order = Func.labels_reverse_postorder func in
  let active_in_table = Label.Table.create () in
  let active_out_table = Label.Table.create () in
  let func =
    let edit = Multi_edit.create () in
    begin
      let@: label = Vec.iter labels_order in
      let block = Func.find_block_exn func label in
      let active_in =
        init_usual
          ~num_regs
          ~preds:pred_table.Label.!(label)
          ~next_use_in:(Liveness.Next_use_table.find next_use_in_table label)
          ~active_out_table
      in
      (* TODO: enable this for chaos mode *)
      (* let active_in = Ident.Set.empty in *)
      let next_use_out = Liveness.Next_use_table.find next_use_out_table label in
      let active_out =
        spill_block ~num_regs ~active_in ~next_use_out ~spiller ~edit block
      in
      active_in_table.Label.!(label) <- active_in;
      active_out_table.Label.!(label) <- active_out
    end;
    Func.apply_multi_edit edit func
  in
  let func =
    fixup_func ~ty_table ~pred_table ~active_in_table ~active_out_table ~spiller func
  in
  let func = Spiller.apply_func spiller func in
  let func = insert_spills ~spiller func in
  let func = Convert_ssa.convert func in
  func
;;
