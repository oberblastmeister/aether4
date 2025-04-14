module C0 = Ae_c0_std
module Lower_flat_x86 = Ae_abs_x86_lower_flat_x86
module Entity = Ae_entity_std
module Regalloc = Ae_abs_x86_regalloc
module Flat_x86 = Ae_flat_x86_std
module Frame_layout = Ae_abs_x86_frame_layout
module Pre_spill = Ae_abs_x86_pre_spill
open Ae_abs_x86_types
open Ae_trace

let convert func =
  let func = Split_critical.split func in
  (* trace_s [%message "before_spill" (func : Func.t)];
  let func = Pre_spill.spill_func ~num_regs:6 func in
  trace_s [%message "after_spill" (func : Func.t)]; *)
  let mach_reg_id, func =
    ( func.next_temp_id
    , { func with next_temp_id = Entity.Id.offset func.next_temp_id Mach_reg.num } )
  in
  let allocation, func = Regalloc.alloc_func ~mach_reg_id func in
  let frame_layout = Frame_layout.compute func in
  let func = Lower_flat_x86.lower ~frame_layout ~allocation func in
  func
;;
