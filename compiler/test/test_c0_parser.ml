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
  [%expect {|
    (Ok
     ((ty (Int 2:5-8)) (name ((t bruh) (span 2:9-13)))
      (block ((block ()) (span [2,16]-[4,6]))) (span [2,5]-[4,6])))
    |}]
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
     ((ty (Int 2:5-8)) (name ((t first) (span 2:9-14)))
      (block
       ((block
         ((Decl
           ((ty (Int 3:7-10)) (name ((t first) (span 3:11-16)))
            (expr
             ((Bin (lhs (Int_const ((t 12) (span 3:19-21)))) (op Add)
               (rhs
                (Bin
                 (lhs
                  (Bin
                   (lhs
                    (Bin (lhs (Int_const ((t 1234) (span 3:24-28)))) (op Mod)
                     (rhs (Int_const ((t 1234) (span 3:31-35)))) (span 3:24-35)))
                   (op Mul) (rhs (Int_const ((t 12) (span 3:38-40))))
                   (span 3:24-40)))
                 (op Div) (rhs (Int_const ((t 2) (span 3:43)))) (span 3:24-44)))
               (span 3:19-44))))
            (span 3:7-44)))
          (Decl
           ((ty (Int 4:7-10)) (name ((t second) (span 4:11-17)))
            (expr
             ((Bin
               (lhs
                (Bin (lhs (Int_const ((t 1234) (span 4:20-24)))) (op Add)
                 (rhs (Int_const ((t 12) (span 4:27-29)))) (span 4:20-29)))
               (op Add) (rhs (Int_const ((t 12) (span 4:32-34)))) (span 4:20-34))))
            (span 4:7-34)))))
        (span [2,17]-[5,6])))
      (span [2,5]-[5,6])))
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
     ((ty (Int 2:5-8)) (name ((t main) (span 2:9-13)))
      (block
       ((block
         ((Decl
           ((ty (Int 3:7-10)) (name ((t i) (span 3:11)))
            (expr ((Int_const ((t 1234) (span 3:15-19))))) (span 3:7-19)))
          (If (cond (Var ((t b) (span 4:11))))
           (body1
            (Block
             ((block
               ((Assign
                 ((lvalue ((t another) (span 5:9-16))) (op Id_assign)
                  (expr (Int_const ((t 1243) (span 5:19-23)))) (span 5:9-23)))))
              (span [4,14]-[6,8]))))
           (body2 ()) (span [4,7]-[6,8]))))
        (span [2,16]-[7,6])))
      (span [2,5]-[7,6])))
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
     ((ty (Int 2:7-10)) (name ((t first) (span 2:11-16)))
      (block
       ((block
         ((Assign
           ((lvalue ((t first) (span 3:9-14))) (op Mul_assign)
            (expr
             (Bin (lhs (Int_const ((t 12) (span 3:18-20)))) (op Add)
              (rhs (Int_const ((t 12) (span 3:23-25)))) (span 3:18-25)))
            (span 3:9-25)))
          (Assign
           ((lvalue ((t another) (span 4:11-18))) (op Mod_assign)
            (expr
             (Bin (lhs (Int_const ((t 12) (span 4:24-26)))) (op Div)
              (rhs (Int_const ((t 12) (span 4:29-31)))) (span 4:24-31)))
            (span 4:11-31)))))
        (span [2,19]-[5,8])))
      (span [2,7]-[5,8])))
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
     ((ty (Int 2:5-8)) (name ((t main) (span 2:9-13)))
      (block
       ((block
         ((Decl
           ((ty (Int 3:11-14)) (name ((t first) (span 3:15-20)))
            (expr ((Int_const ((t 0) (span 3:23))))) (span 3:11-24)))
          (Decl
           ((ty (Int 4:11-14)) (name ((t second) (span 4:15-21)))
            (expr ((Int_const ((t 1234) (span 4:24-28))))) (span 4:11-28)))
          (Decl
           ((ty (Bool 5:11-15)) (name ((t third) (span 5:16-21)))
            (expr ((Bool_const ((t true) (span 5:24-28))))) (span 5:11-28)))
          (Return
           (expr
            (Bin (lhs (Var ((t first) (span 6:18-23)))) (op Add)
             (rhs (Var ((t second) (span 6:26-32)))) (span 6:18-32)))
           (span 6:11-32))))
        (span [2,16]-[7,10])))
      (span [2,5]-[7,10])))
    |}]