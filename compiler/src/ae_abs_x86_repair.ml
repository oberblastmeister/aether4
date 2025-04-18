open Std
open Ae_abs_x86_types
module Mach_reg = Ae_x86_mach_reg
module Entity = Ae_entity_std
module Ident = Entity.Ident

let repair_instr ~edit ~get_mach_reg_temp ~live_in ~live_out instr =
  let live_through_temps =
    Ident.Set.iter live_in |> Iter.filter ~f:(Ident.Set.mem live_out) |> Iter.to_list
  in
  let constrained_uses = Instr.iter_constrained_uses_exn instr |> Iter.to_list in
  let constrained_uses_map = Ident.Map.of_alist_exn constrained_uses in
  let constrained_defs = Instr.iter_constrained_defs_exn instr |> Iter.to_list in
  let constrained_def_mach_regs =
    List.map ~f:snd constrained_defs |> Set.of_list (module Mach_reg)
  in
  let available_mach_regs =
    Set.of_list (module Mach_reg) Call_conv.regalloc_usable_mach_regs |> ref
  in
  let get_register temp =
    (* first try to assign it to the register we already have *)
    let mach_reg = get_mach_reg_temp temp in
    if Set.mem !available_mach_regs mach_reg
    then begin
      available_mach_regs := Set.remove !available_mach_regs mach_reg;
      mach_reg
    end
    else begin
      let mach_reg = Set.min_elt_exn !available_mach_regs in
      available_mach_regs := Set.remove !available_mach_regs mach_reg;
      mach_reg
    end
  in
  let assignment_before, parallel_move_before =
    let assignment_before = ref Ident.Map.empty in
    let parallel_move_before = Lstack.create () in
    List.iter constrained_uses ~f:(fun (temp, mach_reg) ->
      Lstack.push parallel_move_before (mach_reg, temp));
    begin
      let@: live_through_temp = List.iter live_through_temps in
      match Ident.Map.find constrained_uses_map live_through_temp with
      | Some constrained_to ->
        if Set.mem constrained_def_mach_regs constrained_to
        then begin
          let mach_reg = get_register live_through_temp in
          assignment_before
          := Ident.Map.set !assignment_before ~key:live_through_temp ~data:mach_reg;
          Lstack.push parallel_move_before (mach_reg, live_through_temp)
        end
        else begin
          assignment_before
          := Ident.Map.set !assignment_before ~key:live_through_temp ~data:constrained_to
        end
      | None ->
        let mach_reg = get_register live_through_temp in
        assignment_before
        := Ident.Map.set !assignment_before ~key:live_through_temp ~data:mach_reg;
        Lstack.push parallel_move_before (mach_reg, live_through_temp)
    end;
    !assignment_before, Lstack.to_list_rev parallel_move_before
  in
  let assignment_after =
    List.fold constrained_defs ~init:assignment_before ~f:(fun acc (def, mach_reg) ->
      Ident.Map.add_exn acc ~key:def ~data:mach_reg)
  in
  let parallel_move_after = Ident.Map.to_alist assignment_after in
  ()
;;

let repair_block ~edit ~get_mach_reg_temp ~live_in ~live_out block =
  let live_out_ref = ref live_out in
  begin
    let@: instr' = Block.iter_bwd block in
    let instr = instr'.i in
    let live_out = !live_out_ref in
    repair_instr ~edit ~get_mach_reg_temp ~live_in ~live_out instr;
    live_out_ref := Liveness.backwards_transfer instr !live_out_ref
  end;
  ()
;;

let repair_func func =
  (* let module Table = Entity.Ident.Table in
  let open Table.Syntax in *)
  let pred_table = Func.pred_table func in
  let live_in_table, live_out_table = Liveness.compute ~pred_table func in
  let edit = Multi_edit.create () in
  let get_mach_reg_temp _ = todol [%here] in
  begin
    let@: block = Func.iter_blocks func in
    let live_in = Liveness.Live_set.find live_in_table block.label in
    let live_out = Liveness.Live_set.find live_out_table block.label in
    repair_block ~edit ~get_mach_reg_temp ~live_in ~live_out block;
    ()
  end;
  ()
;;
