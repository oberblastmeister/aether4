open Std
open Aether4
module C0 = Ae_c0_std

let check s =
  let tokens = C0.Lexer.tokenize s in
  print_s [%sexp (tokens : C0.Token.t list)]
;;

let%expect_test _ =
  check
    {|
    + 123 * ; sadf return int += {  ) () += *= /= int 0xFFE12 1234 0xdeadbeef
  |};
  [%expect
    {|
    (Plus (Decnum 123) Star Semi (Ident sadf) Return Int PlusEq LBrace RParen
     LParen RParen PlusEq StarEq SlashEq Int (Hexnum 0xFFE12) (Decnum 1234)
     (Hexnum 0xdeadbeef))
    |}]
;;

let%expect_test "comments" =
  check {|
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
  [%expect {| ((Decnum 1324) (Decnum 1234) (Ident poawei)) |}]