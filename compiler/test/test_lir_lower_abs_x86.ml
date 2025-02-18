open Std
open Aether4
module C0 = Ae_c0_std
module Lir = Ae_lir_std
module Tir = Ae_tir_std
module Abs_x86 = Ae_abs_x86_std

let check s =
  let tokens = C0.Lexer.tokenize s in
  let program = C0.Parser.parse tokens |> Result.ok |> Option.value_exn in
  let tir = C0.Lower_tree_ir.lower program in
  let lir = Tir.Lower_lir.lower tir in
  let abs_x86 = Lir.Lower_abs_x86.lower lir in
  print_s [%sexp (abs_x86 : Abs_x86.Func.t)];
  ()
;;
