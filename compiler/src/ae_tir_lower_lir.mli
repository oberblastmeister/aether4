module Tir := Ae_tir_types
module Lir := Ae_lir_types

val lower_func : Tir.Func.t -> Lir.Func.t
val lower_program : Tir.Program.t -> Lir.Program.t
