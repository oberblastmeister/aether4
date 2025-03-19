open Std
open Aether4
module C0 = Ae_c0_std
module Spanned = Ae_spanned

let check s =
  let tokens = C0.Lexer.tokenize s in
  print_s [%sexp (tokens : C0.Token.t Spanned.t list)]
;;

let%expect_test _ =
  check
    {|
    + 123 * ; sadf return int += {  ) () += *= /= int 0xFFE12 1234 0xdeadbeef
  |};
  [%expect
    {|
    (((t Plus) (span 2:5)) ((t (Decnum 123)) (span 2:7-10))
     ((t Star) (span 2:11)) ((t Semi) (span 2:13))
     ((t (Ident sadf)) (span 2:15-19)) ((t Return) (span 2:20-26))
     ((t Int) (span 2:27-30)) ((t PlusEq) (span 2:31-33))
     ((t LBrace) (span 2:34)) ((t RParen) (span 2:37)) ((t LParen) (span 2:39))
     ((t RParen) (span 2:40)) ((t PlusEq) (span 2:42-44))
     ((t StarEq) (span 2:45-47)) ((t SlashEq) (span 2:48-50))
     ((t Int) (span 2:51-54)) ((t (Hexnum 0xFFE12)) (span 2:55-62))
     ((t (Decnum 1234)) (span 2:63-67)) ((t (Hexnum 0xdeadbeef)) (span 2:68-78))
     ((t Eof) (span 3:3)))
    |}]
;;

let%expect_test "comments" =
  check
    {|
    // awefawefwef
    // awefawefawef
    // awefawef
    1324 1234 poawei
    
    /*
    awefawef
    aewf
    /* aweawef */
    /* aefwfeaw */
    
    /*awefa
      /* aweawef
      */
    */
    
    */
    |};
  [%expect
    {|
    (((t (Decnum 1324)) (span 5:5-9)) ((t (Decnum 1234)) (span 5:10-14))
     ((t (Ident poawei)) (span 5:15-21)) ((t Eof) (span 19:5)))
    |}]
;;
