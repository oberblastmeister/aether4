module Tir := Ae_tir
module Lir := Ae_lir

val lower : Tir.Func.t -> Lir.Func.t
