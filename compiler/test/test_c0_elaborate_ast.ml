open Std
open Aether4
module C0 = Ae_c0_std
module Tir = Ae_tir_std
module Lir = Ae_lir_lower_abs_x86
module Abs_x86 = Ae_abs_x86_std
module Flat_x86 = Ae_flat_x86_std

let check s =
  let tokens = C0.Lexer.tokenize s in
  let cst = C0.Parser.parse tokens |> Result.ok |> Option.value_exn in
  let ast = C0.Cst_elaborate_ast.elaborate_program cst in
  print_s [%sexp (ast : C0.Ast.program Or_error.t)]
;;

let%expect_test "smoke" =
  check
    {|
  int main() {
            int first = 0;
            int second = 1234;
            bool third = true;
            return first + second;
          }
 |};
  [%expect
    {|
    (Ok
     ((ty
       (Int
        ((start ((line 2) (col 3) (offset 3)))
         (stop ((line 2) (col 6) (offset 6))))))
      (name main)
      (block
       ((Declare
         (ty
          (Int
           ((start ((line 3) (col 13) (offset 28)))
            (stop ((line 3) (col 16) (offset 31))))))
         (var tmp@1)
         (span
          ((start ((line 3) (col 13) (offset 28)))
           (stop ((line 3) (col 26) (offset 41))))))
        (Assign (lvalue tmp@1)
         (expr
          (Int_const
           ((t 0)
            (span
             ((start ((line 3) (col 25) (offset 40)))
              (stop ((line 3) (col 26) (offset 41))))))))
         (span
          ((start ((line 3) (col 13) (offset 28)))
           (stop ((line 3) (col 26) (offset 41))))))
        (Declare
         (ty
          (Int
           ((start ((line 3) (col 13) (offset 28)))
            (stop ((line 3) (col 16) (offset 31))))))
         (var first@0)
         (span
          ((start ((line 3) (col 13) (offset 28)))
           (stop ((line 3) (col 26) (offset 41))))))
        (Assign (lvalue first@0) (expr (Var (var tmp@1) (ty ())))
         (span
          ((start ((line 3) (col 13) (offset 28)))
           (stop ((line 3) (col 26) (offset 41))))))
        (Declare
         (ty
          (Int
           ((start ((line 4) (col 13) (offset 55)))
            (stop ((line 4) (col 16) (offset 58))))))
         (var tmp@3)
         (span
          ((start ((line 4) (col 13) (offset 55)))
           (stop ((line 4) (col 30) (offset 72))))))
        (Assign (lvalue tmp@3)
         (expr
          (Int_const
           ((t 1234)
            (span
             ((start ((line 4) (col 26) (offset 68)))
              (stop ((line 4) (col 30) (offset 72))))))))
         (span
          ((start ((line 4) (col 13) (offset 55)))
           (stop ((line 4) (col 30) (offset 72))))))
        (Declare
         (ty
          (Int
           ((start ((line 4) (col 13) (offset 55)))
            (stop ((line 4) (col 16) (offset 58))))))
         (var second@2)
         (span
          ((start ((line 4) (col 13) (offset 55)))
           (stop ((line 4) (col 30) (offset 72))))))
        (Assign (lvalue second@2) (expr (Var (var tmp@3) (ty ())))
         (span
          ((start ((line 4) (col 13) (offset 55)))
           (stop ((line 4) (col 30) (offset 72))))))
        (Declare
         (ty
          (Bool
           ((start ((line 5) (col 13) (offset 86)))
            (stop ((line 5) (col 17) (offset 90))))))
         (var tmp@5)
         (span
          ((start ((line 5) (col 13) (offset 86)))
           (stop ((line 5) (col 30) (offset 103))))))
        (Assign (lvalue tmp@5)
         (expr
          (Bool_const
           ((t true)
            (span
             ((start ((line 5) (col 26) (offset 99)))
              (stop ((line 5) (col 30) (offset 103))))))))
         (span
          ((start ((line 5) (col 13) (offset 86)))
           (stop ((line 5) (col 30) (offset 103))))))
        (Declare
         (ty
          (Bool
           ((start ((line 5) (col 13) (offset 86)))
            (stop ((line 5) (col 17) (offset 90))))))
         (var third@4)
         (span
          ((start ((line 5) (col 13) (offset 86)))
           (stop ((line 5) (col 30) (offset 103))))))
        (Assign (lvalue third@4) (expr (Var (var tmp@5) (ty ())))
         (span
          ((start ((line 5) (col 13) (offset 86)))
           (stop ((line 5) (col 30) (offset 103))))))
        (Return
         (expr
          (Bin (lhs (Var (var first@0) (ty ()))) (op Add)
           (rhs (Var (var second@2) (ty ()))) (ty ())
           (span
            ((start ((line 6) (col 20) (offset 124)))
             (stop ((line 6) (col 34) (offset 138)))))))
         (span
          ((start ((line 6) (col 13) (offset 117)))
           (stop ((line 6) (col 34) (offset 138))))))))))
    |}]
;;
