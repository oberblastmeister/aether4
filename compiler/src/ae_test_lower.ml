open Std
module Stack_builder = Ae_stack_builder
module C0 = Ae_c0_std
module Tir = Ae_tir_std
module Lir = Ae_lir_lower_abs_x86
module Abs_x86 = Ae_abs_x86_std
module Flat_x86 = Ae_flat_x86_std

let check s =
  let tokens = C0.Lexer.tokenize s in
  let program = C0.Parser.parse tokens |> Result.ok |> Option.value_exn in
  let program = C0.Cst_elaborate_ast.elaborate_program program |> Or_error.ok_exn in
  let tir = C0.Lower_tree_ir.lower program in
  let lir = Tir.Lower_lir.lower tir in
  let abs_x86 = Lir.lower lir in
  let stack_builder = Stack_builder.create () in
  let alloc = Abs_x86.Regalloc.alloc_func stack_builder abs_x86 in
  let asm = Abs_x86.Lower_flat_x86.lower alloc abs_x86 in
  let formatted_asm = Flat_x86.Format.format asm in
  print_endline formatted_asm;
  ()
;;
