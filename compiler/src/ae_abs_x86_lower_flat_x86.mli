module Abs_x86 := Ae_abs_x86_types
module Flat_x86 := Ae_flat_x86_types
module Regalloc = Ae_abs_x86_regalloc
module Frame := Ae_x86_frame

val lower
  :  Frame.Layout.t
  -> Regalloc.Allocation.t
  -> Abs_x86.Func.t
  -> Flat_x86.Program.t
