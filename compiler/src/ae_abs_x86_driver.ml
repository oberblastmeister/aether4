open Std
module C0 = Ae_c0_std
module Lower_flat_x86 = Ae_abs_x86_lower_flat_x86
module Entity = Ae_entity_std
module Legalize = Ae_abs_x86_legalize
module Regalloc = Ae_abs_x86_regalloc
module Regalloc_treescan = Ae_abs_x86_regalloc_treescan
module Flat_x86 = Ae_flat_x86_std
module Frame_layout = Ae_abs_x86_frame_layout
module Pre_spill = Ae_abs_x86_pre_spill
module Post_spill = Ae_abs_x86_post_spill
module Destruct_ssa = Ae_abs_x86_destruct_ssa
module Repair = Ae_abs_x86_repair
module Check_register_pressure = Ae_abs_x86_check_register_pressure
open Ae_abs_x86_types
open Ae_trace

(*
   (label ir
  (block_params n_23 n_24 n_25)
  (n_23 <- var n_24)
  (rhs_6 <- eq.q lhs_4 rhs_5)
  (rhs_7 <- add.q lhs_4 rhs_5)
  (jump (first rhs_6 rhs_7) (second rhs_6 rhs_7))
  (ret)
)
*)
let mach_reg_id off mach_reg = Entity.Id.offset off (Mach_reg.to_enum mach_reg)

let mach_reg_ident ?info off mach_reg =
  let id = mach_reg_id off mach_reg in
  Entity.Ident.create ?info (Mach_reg.to_string mach_reg) id
;;

let trace_allocation allocation =
  let allocation =
    Entity.Ident.Table.to_list allocation
    |> (List.map & Tuple2.map_snd) ~f:Mach_reg.of_enum_exn
  in
  trace_s [%message (allocation : (Temp.t * Mach_reg.t) list)]
;;

let convert func =
  let module Table = Entity.Ident.Table in
  let open Table.Syntax in
  let func = Legalize.legalize_func func in
  let func = Pre_spill.spill_func ~num_regs:4 func in
  trace_s [%message "spilled" (func : Func.t)];
  Check_register_pressure.check_func ~num_regs:4 func;
  Check.check func |> Or_error.ok_exn;
  (* let allocation, spilled_colors = Regalloc.alloc_func func in *)
  let allocation = Regalloc_treescan.alloc_func func in
  trace_allocation allocation;
  (* if not (Set.is_empty spilled_colors)
  then raise_s [%message (spilled_colors : Int.Set.t)]; *)
  let mach_reg_gen = Func.create_mach_reg_gen ~allocation func in
  let func = Repair.repair_func ~mach_reg_gen ~allocation func in
  (* let func =
    Post_spill.spill_func
      ~mach_reg_gen
      ~get_temp_color:(fun temp -> allocation.!(temp))
      ~spilled_colors
      func
  in *)
  let func = Split_critical.split func in
  let func =
    Destruct_ssa.destruct
      ~mach_reg_gen
      ~in_same_reg:(fun t1 t2 ->
        let conv t =
          Location.to_either t
          |> Either.map ~first:(fun temp -> allocation.!(temp)) ~second:Fn.id
        in
        [%equal: (int, Stack_slot.t) Either.t] (conv t1) (conv t2))
      ~get_scratch:(fun () -> Temp (Mach_reg_gen.get mach_reg_gen R11))
      func
  in
  let frame_layout = Frame_layout.compute func in
  let func = Lower_flat_x86.lower ~frame_layout ~allocation func in
  func
;;
