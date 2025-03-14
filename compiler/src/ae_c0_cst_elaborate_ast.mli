open Std
module Cst := Ae_c0_cst
module Ast := Ae_c0_ast

val elaborate_program : Cst.program -> Ast.program Or_error.t
