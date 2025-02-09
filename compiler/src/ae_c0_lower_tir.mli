open Std
module Cst := Ae_c0_cst
module Tir := Ae_tir_types

val lower : Cst.program -> Tir.Func.t
