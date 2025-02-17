open Std
module Lexer = Ae_c0_lexer_gen
module Token = Ae_c0_token

let tokenize s =
  let lexbuf = Lexing.from_string s in
  let rec loop acc =
    match Lexer.lex lexbuf with
    | Eof -> List.rev acc
    | tok -> loop (tok :: acc)
  in
  loop []
;;