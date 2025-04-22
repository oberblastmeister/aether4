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
open Ae_trace
module Entity = Ae_entity_std
module Ident = Entity.Ident

let check_block ~num_regs ~live_out (block : Block.t) =
  let live_out_ref = ref live_out in
  begin
    let@: instr' = Block.iter_bwd block in
    let instr = instr'.i in
    let live_out = !live_out_ref in
    let live_in = Liveness.backwards_transfer instr live_out in
    let live_through =
      Set.iter live_in |> Iter.filter ~f:(Set.mem live_out) |> Iter.to_list
    in
    let defs = Instr.iter_defs instr |> Iter.to_list in
    let clobbers = Instr.iter_clobbers instr |> Iter.to_list in
    let num_temps = List.length live_through + List.length defs + List.length clobbers in
    if num_temps > num_regs
    then
      raise_s
        [%message
          "Too many live temporaries"
            (block.label : Label.t)
            (instr : Instr.t)
            (live_in : Temp.Set.t)
            (live_out : Temp.Set.t)
            (live_through : Temp.t list)
            (defs : Temp.t list)
            (clobbers : Mach_reg.t list)
            (num_regs : int)
            (num_temps : int)];
    live_out_ref := live_in
  end
;;

let check_func ~num_regs func =
  let pred_table = Func.pred_table func in
  let _live_in_table, live_out_table = Liveness.compute ~pred_table func in
  begin
    let@: block = Func.iter_blocks func in
    let live_out = Liveness.Live_set.find live_out_table block.label in
    check_block ~num_regs ~live_out block
  end
;;
