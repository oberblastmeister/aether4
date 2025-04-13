module C0 = Ae_c0_std
module Lower_flat_x86 = Ae_abs_x86_lower_flat_x86
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
  let alloc, func = Regalloc.alloc_func func in
  let frame_layout = Frame_layout.compute func in
  let func = Lower_flat_x86.lower frame_layout alloc func in
  func
;;
