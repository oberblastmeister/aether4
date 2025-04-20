module Lir := Ae_lir_types
module Abs_x86 := Ae_abs_x86_std

val lower_func : Lir.Func.t -> Abs_x86.Func.t
val lower_program : Lir.Program.t -> Abs_x86.Program.t
