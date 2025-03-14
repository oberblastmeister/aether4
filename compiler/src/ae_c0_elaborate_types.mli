open Std
module Ast := Ae_c0_ast

val check_program : Ast.program -> Ast.program Or_error.t
