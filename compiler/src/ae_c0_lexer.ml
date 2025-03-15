open Std
module Lexer = Ae_c0_lexer_gen
module Token = Ae_c0_token
module Span = Ae_span
module Spanned = Ae_spanned

let tokenize s =
  let lexbuf = Lexing.from_string s in
  let rec loop acc =
    match Lexer.lex lexbuf with
    | Eof -> List.rev acc
    | tok ->
      let start = Lexing.lexeme_start_p lexbuf in
      let stop = Lexing.lexeme_end_p lexbuf in
      let span = Span.of_positions ~start ~stop in
      loop ({ Spanned.t = tok; Spanned.span } :: acc)
  in
  loop []
;;
