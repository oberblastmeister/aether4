open Std
module Lexer = Ae_c0_lexer_gen
module Token = Ae_c0_token
module Span = Ae_span
module Spanned = Ae_spanned

let tokenize s =
  let lexbuf = Lexing.from_string s in
  let rec loop acc =
    let tok = Lexer.lex lexbuf in
    let start = Lexing.lexeme_start_p lexbuf in
    let stop = Lexing.lexeme_end_p lexbuf in
    let span = Span.of_positions ~start ~stop in
    let spanned_tok = { Spanned.t = tok; Spanned.span } in
    match tok with
    | Eof -> List.rev (spanned_tok :: acc)
    | _ -> loop (spanned_tok :: acc)
  in
  loop []
;;
