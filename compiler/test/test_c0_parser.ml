open Std
open Aether4
module C0 = Ae_c0_std

let check s =
  let module Lexer = Ae_c0_lexer in
  let tokens = Lexer.tokenize s in
  let program = C0.Parser.parse tokens in
  print_s [%sexp (program : C0.Cst.program Or_error.t)];
  ()
;;

let%expect_test "simple" =
  check
    {|
    int bruh() {
    
    }
  |};
  [%expect {| (Ok ((ty Int) (name bruh) (block ((stmts ()))))) |}]
;;

let%expect_test "simple decl" =
  check
    {|
    int first() {
      int first = 12 + 1234 % 1234 * 12 / 2;
      int second = 1234 + 12 + 12;
    }
  |};
  [%expect
    {|
    (Ok
     ((ty Int) (name first)
      (block
       ((stmts
         ((Decl
           ((ty Int) (name first)
            (expr
             ((Bin (lhs (Int_const 12)) (op Add)
               (rhs
                (Bin
                 (lhs
                  (Bin
                   (lhs
                    (Bin (lhs (Int_const 1234)) (op Mod) (rhs (Int_const 1234))))
                   (op Mul) (rhs (Int_const 12))))
                 (op Div) (rhs (Int_const 2)))))))))
          (Decl
           ((ty Int) (name second)
            (expr
             ((Bin
               (lhs (Bin (lhs (Int_const 1234)) (op Add) (rhs (Int_const 12))))
               (op Add) (rhs (Int_const 12)))))))))))))
    |}]
;;

let%expect_test "simple control flow" =
  check
    {|
    int main() {
      int i = 1234;
      if (b) {
        another = 1243;
      }
    }
  |};
  [%expect
    {|
    (Ok
     ((ty Int) (name main)
      (block
       ((stmts
         ((Decl ((ty Int) (name i) (expr ((Int_const 1234)))))
          (If (cond (Var b))
           (body1
            (Block
             ((stmts
               ((Assign ((lvalue another) (op Eq) (expr (Int_const 1243)))))))))
           (body2 ()))))))))
    |}]
;;

let%expect_test "simple assign" =
  check
    {|
      int first() {
        first *= 12 + 12;
        ((another)) %= 12 / 12;      
      }
    |};
  [%expect
    {|
    (Ok
     ((ty Int) (name first)
      (block
       ((stmts
         ((Assign
           ((lvalue first) (op Mul_eq)
            (expr (Bin (lhs (Int_const 12)) (op Add) (rhs (Int_const 12))))))
          (Assign
           ((lvalue another) (op Mod_eq)
            (expr (Bin (lhs (Int_const 12)) (op Div) (rhs (Int_const 12))))))))))))
    |}]
;;

let%expect_test "bool" =
  check {|
    int main() {
          int first = 0;
          int second = 1234;
          bool third = true;
          return first + second;
        }
  |};
  [%expect {|
    (Ok
     ((ty Int) (name main)
      (block
       ((stmts
         ((Decl ((ty Int) (name first) (expr ((Int_const 0)))))
          (Decl ((ty Int) (name second) (expr ((Int_const 1234)))))
          (Decl ((ty Bool) (name third) (expr ((Bool_const true)))))
          (Return (Bin (lhs (Var first)) (op Add) (rhs (Var second))))))))))
    |}]