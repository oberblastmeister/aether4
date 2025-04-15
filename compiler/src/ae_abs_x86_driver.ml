module C0 = Ae_c0_std
module Lower_flat_x86 = Ae_abs_x86_lower_flat_x86
module Entity = Ae_entity_std
module Regalloc = Ae_abs_x86_regalloc
module Flat_x86 = Ae_flat_x86_std
module Frame_layout = Ae_abs_x86_frame_layout
module Pre_spill = Ae_abs_x86_pre_spill
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
let convert func =
  let func = Split_critical.split func in
  let func = Pre_spill.spill_func ~num_regs:8 func in
  let mach_reg_id, func =
    ( func.next_temp_id
    , { func with next_temp_id = Entity.Id.offset func.next_temp_id Mach_reg.num } )
  in
  let allocation, func = Regalloc.alloc_func ~mach_reg_id func in
  let frame_layout = Frame_layout.compute func in
  let func = Lower_flat_x86.lower ~frame_layout ~allocation func in
  func
;;
