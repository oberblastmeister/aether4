open Std
open Ae_abs_x86_types
module Mach_reg = Ae_x86_mach_reg
module Entity = Ae_entity_std
module Ident = Entity.Ident
open Ae_trace

module Sequentialize_parallel_moves = Ae_sequentialize_parallel_moves.Make (struct
    module Temp = Temp
    module Ty = Ty
  end)

(*
   this works because we only duplicate a live through temporary when its color conflicts
  with a definition that is constrained
*)
let repair_instr ~mach_reg_gen ~ty_table ~get_temp_mach_reg ~live_in ~live_out instr =
  let open Ident.Table.Syntax in
  let constrained_uses = Instr.iter_constrained_uses_exn instr |> Iter.to_list in
  let constrained_defs = Instr.iter_constrained_defs_exn instr |> Iter.to_list in
  if List.is_empty constrained_uses && List.is_empty constrained_defs
  then [], instr, []
  else begin
    let uses = Instr.iter_uses instr |> Iter.to_list in
    let defs = Instr.iter_defs instr |> Iter.to_list in
    let live_through =
      Ident.Set.iter live_in |> Iter.filter ~f:(Ident.Set.mem live_out) |> Iter.to_list
    in
    let constrained_uses_map = Ident.Map.of_alist_exn constrained_uses in
    let constrained_defs_map = Ident.Map.of_alist_exn constrained_defs in
    let clobbers = Instr.iter_clobbers instr |> Iter.to_list in
    let used_mach_regs =
      List.map ~f:snd constrained_uses |> Set.of_list (module Mach_reg)
    in
    let defined_mach_regs =
      List.map ~f:snd constrained_defs @ clobbers |> Set.of_list (module Mach_reg)
    in
    let alloc_register available_mach_regs temp =
      (* first try to assign it to the register we already have *)
      let mach_reg = get_temp_mach_reg temp in
      if Set.mem !available_mach_regs mach_reg
      then begin
        available_mach_regs := Set.remove !available_mach_regs mach_reg;
        mach_reg
      end
      else begin
        let mach_reg =
          Set.min_elt !available_mach_regs
          |> Option.value_exn
               ~message:
                 "Should have run pre spill before this so the number of live variables \
                  is reduced"
        in
        available_mach_regs := Set.remove !available_mach_regs mach_reg;
        mach_reg
      end
    in
    let available_mach_regs =
      Set.of_list (module Mach_reg) Call_conv.regalloc_usable_mach_regs
      |> Set.diff __ defined_mach_regs
      |> Set.diff __ used_mach_regs
      |> ref
    in
    let parallel_moves_before = Lstack.create () in
    (* first allocate constrained uses *)
    let allocation = ref Ident.Map.empty in
    List.iter constrained_uses ~f:(fun (temp, mach_reg) ->
      allocation := Ident.Map.add_exn !allocation ~key:temp ~data:mach_reg;
      Lstack.push parallel_moves_before (mach_reg, temp));
    (* then allocate live through temps **)
    begin
      let@: live_through = List.iter live_through in
      match Ident.Map.find constrained_uses_map live_through with
      | Some constrained_to ->
        assert (Ident.Map.mem !allocation live_through);
        if Set.mem defined_mach_regs constrained_to
        then begin
          (* now duplicate *)
          let mach_reg = alloc_register available_mach_regs live_through in
          (* overwrite the allocation to a copy *)
          allocation := Ident.Map.set !allocation ~key:live_through ~data:mach_reg;
          Lstack.push parallel_moves_before (mach_reg, live_through)
        end
        else begin
          (* don't need to move it, because we already moved all constrained uses above *)
          assert (
            Mach_reg.equal (Ident.Map.find_exn !allocation live_through) constrained_to)
        end
      | None ->
        let mach_reg = alloc_register available_mach_regs live_through in
        allocation := Ident.Map.set !allocation ~key:live_through ~data:mach_reg;
        Lstack.push parallel_moves_before (mach_reg, live_through)
    end;
    (* then allocate all used temps that died here *)
    begin
      (*
         We locally have available_mach_regs because the uses die here.
      *)
      let available_mach_regs =
        !available_mach_regs
        |> Set.union (Set.diff defined_mach_regs used_mach_regs)
        |> ref
      in
      let@: last_use =
        List.iter uses |> Iter.filter ~f:(fun use -> not (Ident.Map.mem !allocation use))
      in
      assert (not (Ident.Set.mem live_out last_use));
      assert (not (Ident.Map.mem constrained_uses_map last_use));
      let mach_reg = alloc_register available_mach_regs last_use in
      allocation := Ident.Map.add_exn !allocation ~key:last_use ~data:mach_reg;
      Lstack.push parallel_moves_before (mach_reg, last_use)
    end;
    let parallel_moves_before = Lstack.to_list_rev parallel_moves_before in
    List.iter constrained_defs ~f:(fun (def, mach_reg) ->
      allocation := Ident.Map.add_exn !allocation ~key:def ~data:mach_reg);
    begin
      let@: def =
        List.iter defs
        |> Iter.filter ~f:(fun def -> not (Ident.Map.mem constrained_defs_map def))
      in
      let mach_reg = alloc_register available_mach_regs def in
      allocation := Ident.Map.add_exn !allocation ~key:def ~data:mach_reg
    end;
    let allocation = !allocation in
    let parallel_moves_after =
      Ident.Map.to_alist allocation
      (* very important!
      We must do this because uses that die here have registers that are reused by the register allocation with some def.
      This would cause a parallel move to have duplicate move destinations, which is incorrect.
      *)
      |> List.filter ~f:(fun (temp, _) -> Ident.Set.mem live_out temp)
    in
    let in_same_reg t1 t2 =
      Mach_reg.equal (get_temp_mach_reg t1) (get_temp_mach_reg t2)
    in
    let get_scratch () = Mach_reg_gen.get mach_reg_gen R11 in
    let sequentialize =
      Sequentialize_parallel_moves.sequentialize ~in_same_reg ~get_scratch
    in
    let sequential_moves_before, _ =
      let parallel_moves_before =
        List.map parallel_moves_before ~f:(fun (mach_reg, temp) ->
          { Sequentialize_parallel_moves.Move.dst = Mach_reg_gen.get mach_reg_gen mach_reg
          ; src = temp
          ; ty = ty_table.!(temp)
          })
      in
      sequentialize parallel_moves_before
    in
    let sequential_moves_after, _ =
      let parallel_moves_after =
        List.map parallel_moves_after ~f:(fun (temp, mach_reg) ->
          { Sequentialize_parallel_moves.Move.dst = temp
          ; src = Mach_reg_gen.get mach_reg_gen mach_reg
          ; ty = ty_table.!(temp)
          })
      in
      sequentialize parallel_moves_after
    in
    let convert moves =
      List.map moves ~f:(fun { Sequentialize_parallel_moves.Move.dst; src; ty } ->
        Instr.Mov { dst = Reg dst; src = Reg src; size = ty })
    in
    let new_instr =
      let@: temp = Instr.map_uses instr in
      Mach_reg_gen.get mach_reg_gen (Ident.Map.find_exn allocation temp)
    in
    let new_instr =
      let@: temp = Instr.map_defs new_instr in
      Mach_reg_gen.get mach_reg_gen (Ident.Map.find_exn allocation temp)
    in
    convert sequential_moves_before, new_instr, convert sequential_moves_after
  end
;;

let repair_block ~edit ~mach_reg_gen ~ty_table ~get_temp_mach_reg ~live_out block =
  let live_out_ref = ref live_out in
  begin
    let@: instr' = Block.iter_bwd block in
    let instr = instr'.i in
    let live_out = !live_out_ref in
    let live_in = Liveness.backwards_transfer instr live_out in
    let moves_before, new_instr, moves_after =
      repair_instr ~mach_reg_gen ~ty_table ~get_temp_mach_reg ~live_in ~live_out instr
    in
    Multi_edit.add_inserts
      edit
      block.label
      (List.map moves_before ~f:(fun instr ->
         Instr'.create ~info:(Info.create_s [%message "repair"]) instr instr'.index));
    Multi_edit.add_replace
      edit
      block.label
      (Instr'.create ?info:instr'.info new_instr instr'.index);
    Multi_edit.add_inserts
      edit
      block.label
      (List.map moves_after ~f:(fun instr ->
         Instr'.create ~info:(Info.create_s [%message "repair"]) instr (instr'.index + 1)));
    live_out_ref := live_in
  end;
  ()
;;

let repair_func ~allocation ~mach_reg_gen func =
  let ty_table = Func.get_ty_table func in
  let pred_table = Func.pred_table func in
  let _live_in_table, live_out_table = Liveness.compute ~pred_table func in
  let edit = Multi_edit.create () in
  let get_temp_mach_reg temp =
    Entity.Ident.Table.find_exn allocation temp |> Mach_reg.of_enum_exn
  in
  begin
    let@: block = Func.iter_blocks func in
    let live_out = Liveness.Live_set.find live_out_table block.label in
    repair_block ~edit ~ty_table ~mach_reg_gen ~get_temp_mach_reg ~live_out block;
    ()
  end;
  Func.apply_multi_edit edit func
;;
