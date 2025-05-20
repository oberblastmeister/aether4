open Std
open Ae_c0_ast

val expr_free_vars : expr -> Var.Set.t
val stmt_free_vars : stmt -> Var.Set.t
val block_free_vars : block -> Var.Set.t
