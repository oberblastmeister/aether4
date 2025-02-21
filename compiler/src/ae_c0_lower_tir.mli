open Std
module Ast := Ae_c0_ast
module Tir := Ae_tir_types

val lower : Ast.program -> Tir.Func.t
