module Tir := Ae_tir_types
module Lir := Ae_lir_types

val lower : Tir.Func.t -> Lir.Func.t
