open Std
open Aether4
module C0 = Ae_c0_std
module Tir = Ae_tir_std

let check s =
  let tokens = C0.Lexer.tokenize s in
  let program = C0.Parser.parse tokens |> Result.ok |> Option.value_exn in
  let tir = C0.Lower_tree_ir.lower program in
  print_s [%sexp (tir : Tir.Func.t)];
  ()
;;
