module Abs_x86 := Ae_abs_x86_types
module Flat_x86 := Ae_flat_x86_types
module Regalloc = Ae_abs_x86_regalloc
module Frame_layout = Ae_abs_x86_frame_layout

val c0_main_export_instructions : Flat_x86.Line.t list

val lower
  :  frame_layout:Frame_layout.t
  -> allocation:int Abs_x86.Temp.Table.t
  -> Abs_x86.Func.t
  -> Flat_x86.Program.t
