open Std
module C0 = Ae_c0_std
module Lower_flat_x86 = Ae_abs_x86_lower_flat_x86
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

let trace_allocation allocation =
  let allocation =
    Temp.Table.to_list allocation |> (List.map & Tuple2.map_snd) ~f:Mach_reg.of_enum_exn
  in
  trace_s [%message (allocation : (Temp.t * Mach_reg.t) list)]
;;

let convert ~func_index func =
  let func = Legalize.legalize_func func in
  let func = Pre_spill.spill_func ~num_regs:Call_conv.num_regs func in
  Check_register_pressure.check_func ~num_regs:Call_conv.num_regs func;
  Check.check func |> Or_error.ok_exn;
  let allocation =
    Regalloc_treescan.alloc_func ~colors:Call_conv.regalloc_usable_colors func
  in
  let mach_reg_gen = Func.create_mach_reg_gen ~allocation func in
  let func = Repair.repair_func ~mach_reg_gen ~allocation func in
  let func = Split_critical.split func in
  let func =
    Destruct_ssa.destruct
      ~mach_reg_gen
      ~in_same_reg:(fun t1 t2 ->
        let conv t =
          Location.to_either t
          |> Either.map
               ~first:(fun temp -> allocation.Temp.!(temp))
               ~second:Fn.id
        in
        [%equal: (int, Stack_address.t) Either.t] (conv t1) (conv t2))
      ~get_scratch:(fun () -> Temp (Mach_reg_gen.get mach_reg_gen R11))
      func
  in
  let frame_layout = Frame_layout.compute func in
  let func = Lower_flat_x86.lower ~frame_layout ~allocation ~func_index func in
  func
;;

let convert_program (program : Program.t) =
  let funcs =
    List.concat_mapi program.funcs ~f:(fun func_index func -> convert ~func_index func)
  in
  Flat_x86.Line.[ Directive ".text" ] @ Lower_flat_x86.c0_main_export_instructions @ funcs
;;
