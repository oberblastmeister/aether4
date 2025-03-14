open Std
module Ast := Ae_c0_ast

val check_program : Ast.program -> unit Or_error.t
