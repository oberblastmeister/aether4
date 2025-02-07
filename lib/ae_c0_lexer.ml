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

let check s =
  let tokens = tokenize s in
  print_s [%sexp (tokens : Token.t list)]
;;

let%expect_test _ =
  check
    {|
    + 123 * ; sadf return int += {  ) () += *= /= int 0xFFE12 1234 0xdeadbeef
  |};
  [%expect {|
    (Plus (Decnum 123) Star Semi (Ident sadf) Return Int PlusEq LBrace RParen
     LParen RParen PlusEq StarEq SlashEq Int (Hexnum 0xFFE12) (Decnum 1234)
     (Hexnum 0xdeadbeef))
    |}]
;;
