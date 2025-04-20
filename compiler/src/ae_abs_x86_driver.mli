module C0 := Ae_c0_std
module Abs_x86 := Ae_abs_x86_types
module Flat_x86 := Ae_flat_x86_std

val convert : Abs_x86.Func.t -> Flat_x86.Program.t
val convert_program : Abs_x86.Program.t -> Flat_x86.Program.t
