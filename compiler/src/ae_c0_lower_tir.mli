open Std
module Ast := Ae_c0_ast
module Tir := Ae_tir_types

val lower_program : Ast.program -> Tir.Program.t
