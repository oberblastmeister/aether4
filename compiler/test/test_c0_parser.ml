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
     ((ty
       (Int
        ((start ((line 2) (col 5) (offset 5)))
         (stop ((line 2) (col 8) (offset 8))))))
      (name
       ((t bruh)
        (span
         ((start ((line 2) (col 9) (offset 9)))
          (stop ((line 2) (col 13) (offset 13)))))))
      (block
       ((block ())
        (span
         ((start ((line 2) (col 16) (offset 16)))
          (stop ((line 4) (col 6) (offset 28)))))))))
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
     ((ty
       (Int
        ((start ((line 2) (col 5) (offset 5)))
         (stop ((line 2) (col 8) (offset 8))))))
      (name
       ((t first)
        (span
         ((start ((line 2) (col 9) (offset 9)))
          (stop ((line 2) (col 14) (offset 14)))))))
      (block
       ((block
         ((Decl
           ((ty
             (Int
              ((start ((line 3) (col 7) (offset 25)))
               (stop ((line 3) (col 10) (offset 28))))))
            (name
             ((t first)
              (span
               ((start ((line 3) (col 11) (offset 29)))
                (stop ((line 3) (col 16) (offset 34)))))))
            (expr
             ((Bin
               (lhs
                (Int_const
                 ((t 12)
                  (span
                   ((start ((line 3) (col 19) (offset 37)))
                    (stop ((line 3) (col 21) (offset 39))))))))
               (op Add)
               (rhs
                (Bin
                 (lhs
                  (Bin
                   (lhs
                    (Bin
                     (lhs
                      (Int_const
                       ((t 1234)
                        (span
                         ((start ((line 3) (col 24) (offset 42)))
                          (stop ((line 3) (col 28) (offset 46))))))))
                     (op Mod)
                     (rhs
                      (Int_const
                       ((t 1234)
                        (span
                         ((start ((line 3) (col 31) (offset 49)))
                          (stop ((line 3) (col 35) (offset 53))))))))
                     (span
                      ((start ((line 3) (col 24) (offset 42)))
                       (stop ((line 3) (col 35) (offset 53)))))))
                   (op Mul)
                   (rhs
                    (Int_const
                     ((t 12)
                      (span
                       ((start ((line 3) (col 38) (offset 56)))
                        (stop ((line 3) (col 40) (offset 58))))))))
                   (span
                    ((start ((line 3) (col 24) (offset 42)))
                     (stop ((line 3) (col 40) (offset 58)))))))
                 (op Div)
                 (rhs
                  (Int_const
                   ((t 2)
                    (span
                     ((start ((line 3) (col 43) (offset 61)))
                      (stop ((line 3) (col 44) (offset 62))))))))
                 (span
                  ((start ((line 3) (col 24) (offset 42)))
                   (stop ((line 3) (col 44) (offset 62)))))))
               (span
                ((start ((line 3) (col 19) (offset 37)))
                 (stop ((line 3) (col 44) (offset 62))))))))
            (span
             ((start ((line 3) (col 7) (offset 25)))
              (stop ((line 3) (col 44) (offset 62)))))))
          (Decl
           ((ty
             (Int
              ((start ((line 4) (col 7) (offset 70)))
               (stop ((line 4) (col 10) (offset 73))))))
            (name
             ((t second)
              (span
               ((start ((line 4) (col 11) (offset 74)))
                (stop ((line 4) (col 17) (offset 80)))))))
            (expr
             ((Bin
               (lhs
                (Bin
                 (lhs
                  (Int_const
                   ((t 1234)
                    (span
                     ((start ((line 4) (col 20) (offset 83)))
                      (stop ((line 4) (col 24) (offset 87))))))))
                 (op Add)
                 (rhs
                  (Int_const
                   ((t 12)
                    (span
                     ((start ((line 4) (col 27) (offset 90)))
                      (stop ((line 4) (col 29) (offset 92))))))))
                 (span
                  ((start ((line 4) (col 20) (offset 83)))
                   (stop ((line 4) (col 29) (offset 92)))))))
               (op Add)
               (rhs
                (Int_const
                 ((t 12)
                  (span
                   ((start ((line 4) (col 32) (offset 95)))
                    (stop ((line 4) (col 34) (offset 97))))))))
               (span
                ((start ((line 4) (col 20) (offset 83)))
                 (stop ((line 4) (col 34) (offset 97))))))))
            (span
             ((start ((line 4) (col 7) (offset 70)))
              (stop ((line 4) (col 34) (offset 97)))))))))
        (span
         ((start ((line 2) (col 17) (offset 17)))
          (stop ((line 5) (col 6) (offset 104)))))))))
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
     ((ty
       (Int
        ((start ((line 2) (col 5) (offset 5)))
         (stop ((line 2) (col 8) (offset 8))))))
      (name
       ((t main)
        (span
         ((start ((line 2) (col 9) (offset 9)))
          (stop ((line 2) (col 13) (offset 13)))))))
      (block
       ((block
         ((Decl
           ((ty
             (Int
              ((start ((line 3) (col 7) (offset 24)))
               (stop ((line 3) (col 10) (offset 27))))))
            (name
             ((t i)
              (span
               ((start ((line 3) (col 11) (offset 28)))
                (stop ((line 3) (col 12) (offset 29)))))))
            (expr
             ((Int_const
               ((t 1234)
                (span
                 ((start ((line 3) (col 15) (offset 32)))
                  (stop ((line 3) (col 19) (offset 36)))))))))
            (span
             ((start ((line 3) (col 7) (offset 24)))
              (stop ((line 3) (col 19) (offset 36)))))))
          (If
           (cond
            (Var
             ((t b)
              (span
               ((start ((line 4) (col 11) (offset 48)))
                (stop ((line 4) (col 12) (offset 49))))))))
           (body1
            (Block
             ((block
               ((Assign
                 ((lvalue
                   ((t another)
                    (span
                     ((start ((line 5) (col 9) (offset 61)))
                      (stop ((line 5) (col 16) (offset 68)))))))
                  (op Id_assign)
                  (expr
                   (Int_const
                    ((t 1243)
                     (span
                      ((start ((line 5) (col 19) (offset 71)))
                       (stop ((line 5) (col 23) (offset 75))))))))
                  (span
                   ((start ((line 5) (col 9) (offset 61)))
                    (stop ((line 5) (col 23) (offset 75)))))))))
              (span
               ((start ((line 4) (col 14) (offset 51)))
                (stop ((line 6) (col 8) (offset 84))))))))
           (body2 ())
           (span
            ((start ((line 4) (col 7) (offset 44)))
             (stop ((line 6) (col 8) (offset 84))))))))
        (span
         ((start ((line 2) (col 16) (offset 16)))
          (stop ((line 7) (col 6) (offset 90)))))))))
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
     ((ty
       (Int
        ((start ((line 2) (col 7) (offset 7)))
         (stop ((line 2) (col 10) (offset 10))))))
      (name
       ((t first)
        (span
         ((start ((line 2) (col 11) (offset 11)))
          (stop ((line 2) (col 16) (offset 16)))))))
      (block
       ((block
         ((Assign
           ((lvalue
             ((t first)
              (span
               ((start ((line 3) (col 9) (offset 29)))
                (stop ((line 3) (col 14) (offset 34)))))))
            (op Mul_assign)
            (expr
             (Bin
              (lhs
               (Int_const
                ((t 12)
                 (span
                  ((start ((line 3) (col 18) (offset 38)))
                   (stop ((line 3) (col 20) (offset 40))))))))
              (op Add)
              (rhs
               (Int_const
                ((t 12)
                 (span
                  ((start ((line 3) (col 23) (offset 43)))
                   (stop ((line 3) (col 25) (offset 45))))))))
              (span
               ((start ((line 3) (col 18) (offset 38)))
                (stop ((line 3) (col 25) (offset 45)))))))
            (span
             ((start ((line 3) (col 9) (offset 29)))
              (stop ((line 3) (col 25) (offset 45)))))))
          (Assign
           ((lvalue
             ((t another)
              (span
               ((start ((line 4) (col 11) (offset 57)))
                (stop ((line 4) (col 18) (offset 64)))))))
            (op Mod_assign)
            (expr
             (Bin
              (lhs
               (Int_const
                ((t 12)
                 (span
                  ((start ((line 4) (col 24) (offset 70)))
                   (stop ((line 4) (col 26) (offset 72))))))))
              (op Div)
              (rhs
               (Int_const
                ((t 12)
                 (span
                  ((start ((line 4) (col 29) (offset 75)))
                   (stop ((line 4) (col 31) (offset 77))))))))
              (span
               ((start ((line 4) (col 24) (offset 70)))
                (stop ((line 4) (col 31) (offset 77)))))))
            (span
             ((start ((line 4) (col 11) (offset 57)))
              (stop ((line 4) (col 31) (offset 77)))))))))
        (span
         ((start ((line 2) (col 19) (offset 19)))
          (stop ((line 5) (col 8) (offset 92)))))))))
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
     ((ty
       (Int
        ((start ((line 2) (col 5) (offset 5)))
         (stop ((line 2) (col 8) (offset 8))))))
      (name
       ((t main)
        (span
         ((start ((line 2) (col 9) (offset 9)))
          (stop ((line 2) (col 13) (offset 13)))))))
      (block
       ((block
         ((Decl
           ((ty
             (Int
              ((start ((line 3) (col 11) (offset 28)))
               (stop ((line 3) (col 14) (offset 31))))))
            (name
             ((t first)
              (span
               ((start ((line 3) (col 15) (offset 32)))
                (stop ((line 3) (col 20) (offset 37)))))))
            (expr
             ((Int_const
               ((t 0)
                (span
                 ((start ((line 3) (col 23) (offset 40)))
                  (stop ((line 3) (col 24) (offset 41)))))))))
            (span
             ((start ((line 3) (col 11) (offset 28)))
              (stop ((line 3) (col 24) (offset 41)))))))
          (Decl
           ((ty
             (Int
              ((start ((line 4) (col 11) (offset 53)))
               (stop ((line 4) (col 14) (offset 56))))))
            (name
             ((t second)
              (span
               ((start ((line 4) (col 15) (offset 57)))
                (stop ((line 4) (col 21) (offset 63)))))))
            (expr
             ((Int_const
               ((t 1234)
                (span
                 ((start ((line 4) (col 24) (offset 66)))
                  (stop ((line 4) (col 28) (offset 70)))))))))
            (span
             ((start ((line 4) (col 11) (offset 53)))
              (stop ((line 4) (col 28) (offset 70)))))))
          (Decl
           ((ty
             (Bool
              ((start ((line 5) (col 11) (offset 82)))
               (stop ((line 5) (col 15) (offset 86))))))
            (name
             ((t third)
              (span
               ((start ((line 5) (col 16) (offset 87)))
                (stop ((line 5) (col 21) (offset 92)))))))
            (expr
             ((Bool_const
               ((t true)
                (span
                 ((start ((line 5) (col 24) (offset 95)))
                  (stop ((line 5) (col 28) (offset 99)))))))))
            (span
             ((start ((line 5) (col 11) (offset 82)))
              (stop ((line 5) (col 28) (offset 99)))))))
          (Return
           (expr
            (Bin
             (lhs
              (Var
               ((t first)
                (span
                 ((start ((line 6) (col 18) (offset 118)))
                  (stop ((line 6) (col 23) (offset 123))))))))
             (op Add)
             (rhs
              (Var
               ((t second)
                (span
                 ((start ((line 6) (col 26) (offset 126)))
                  (stop ((line 6) (col 32) (offset 132))))))))
             (span
              ((start ((line 6) (col 18) (offset 118)))
               (stop ((line 6) (col 32) (offset 132)))))))
           (span
            ((start ((line 6) (col 11) (offset 111)))
             (stop ((line 6) (col 32) (offset 132))))))))
        (span
         ((start ((line 2) (col 16) (offset 16)))
          (stop ((line 7) (col 10) (offset 143)))))))))
    |}]