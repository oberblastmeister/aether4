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

let%expect_test "simple" =
  check
    {|
    int bruh() {
    
    }
  |};
  [%expect
    {|
    ((name bruh)
     (blocks
      ((0
        ((key start@0)
         (data ((body ((BlockParams (temps ())) (Ret (IntConst 0))))))))))
     (start start@0) (next_id 0))
    |}]
;;

let%expect_test "simple decl" =
  check
    {|
    int first() {
      int first = 12 + 1234 % 1234 * 12 / 2;
      int second = first + 12;
      second += first + second;
    }
  |};
  [%expect
    {|
    ((name first)
     (blocks
      ((0
        ((key start@0)
         (data
          ((body
            ((BlockParams (temps ()))
             (Assign (temp first@0)
              (e
               (Bin (lhs (IntConst 12)) (op Add)
                (rhs
                 (Bin
                  (lhs
                   (Bin
                    (lhs
                     (Bin (lhs (IntConst 1234)) (op Mod) (rhs (IntConst 1234))))
                    (op Mul) (rhs (IntConst 12))))
                  (op Div) (rhs (IntConst 2)))))))
             (Assign (temp second@1)
              (e (Bin (lhs (Temp first@0)) (op Add) (rhs (IntConst 12)))))
             (Assign (temp second@1)
              (e
               (Bin (lhs (Temp second@1)) (op Add)
                (rhs (Bin (lhs (Temp first@0)) (op Add) (rhs (Temp second@1)))))))
             (Ret (IntConst 0))))))))))
     (start start@0) (next_id 2))
    |}]
;;
