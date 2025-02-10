module Lir := Ae_lir_types
module Pre_x86 := Ae_pre_x86_std

val lower : Lir.Func.t -> Pre_x86.Func.t
