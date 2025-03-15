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
    (((t Plus)
      (span
       ((start ((line 2) (col 5) (offset 5)))
        (stop ((line 2) (col 6) (offset 6))))))
     ((t (Decnum 123))
      (span
       ((start ((line 2) (col 7) (offset 7)))
        (stop ((line 2) (col 10) (offset 10))))))
     ((t Star)
      (span
       ((start ((line 2) (col 11) (offset 11)))
        (stop ((line 2) (col 12) (offset 12))))))
     ((t Semi)
      (span
       ((start ((line 2) (col 13) (offset 13)))
        (stop ((line 2) (col 14) (offset 14))))))
     ((t (Ident sadf))
      (span
       ((start ((line 2) (col 15) (offset 15)))
        (stop ((line 2) (col 19) (offset 19))))))
     ((t Return)
      (span
       ((start ((line 2) (col 20) (offset 20)))
        (stop ((line 2) (col 26) (offset 26))))))
     ((t Int)
      (span
       ((start ((line 2) (col 27) (offset 27)))
        (stop ((line 2) (col 30) (offset 30))))))
     ((t PlusEq)
      (span
       ((start ((line 2) (col 31) (offset 31)))
        (stop ((line 2) (col 33) (offset 33))))))
     ((t LBrace)
      (span
       ((start ((line 2) (col 34) (offset 34)))
        (stop ((line 2) (col 35) (offset 35))))))
     ((t RParen)
      (span
       ((start ((line 2) (col 37) (offset 37)))
        (stop ((line 2) (col 38) (offset 38))))))
     ((t LParen)
      (span
       ((start ((line 2) (col 39) (offset 39)))
        (stop ((line 2) (col 40) (offset 40))))))
     ((t RParen)
      (span
       ((start ((line 2) (col 40) (offset 40)))
        (stop ((line 2) (col 41) (offset 41))))))
     ((t PlusEq)
      (span
       ((start ((line 2) (col 42) (offset 42)))
        (stop ((line 2) (col 44) (offset 44))))))
     ((t StarEq)
      (span
       ((start ((line 2) (col 45) (offset 45)))
        (stop ((line 2) (col 47) (offset 47))))))
     ((t SlashEq)
      (span
       ((start ((line 2) (col 48) (offset 48)))
        (stop ((line 2) (col 50) (offset 50))))))
     ((t Int)
      (span
       ((start ((line 2) (col 51) (offset 51)))
        (stop ((line 2) (col 54) (offset 54))))))
     ((t (Hexnum 0xFFE12))
      (span
       ((start ((line 2) (col 55) (offset 55)))
        (stop ((line 2) (col 62) (offset 62))))))
     ((t (Decnum 1234))
      (span
       ((start ((line 2) (col 63) (offset 63)))
        (stop ((line 2) (col 67) (offset 67))))))
     ((t (Hexnum 0xdeadbeef))
      (span
       ((start ((line 2) (col 68) (offset 68)))
        (stop ((line 2) (col 78) (offset 78)))))))
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
    (((t (Decnum 1324))
      (span
       ((start ((line 5) (col 5) (offset 60)))
        (stop ((line 5) (col 9) (offset 64))))))
     ((t (Decnum 1234))
      (span
       ((start ((line 5) (col 10) (offset 65)))
        (stop ((line 5) (col 14) (offset 69))))))
     ((t (Ident poawei))
      (span
       ((start ((line 5) (col 15) (offset 70)))
        (stop ((line 5) (col 21) (offset 76)))))))
    |}]
;;
