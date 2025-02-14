module Abs_x86 := Ae_abs_x86_types
module Flat_x86 := Ae_flat_x86_types
module Regalloc = Ae_abs_x86_regalloc

val lower : Regalloc.Allocation.t -> Abs_x86.Func.t -> Flat_x86.Program.t
