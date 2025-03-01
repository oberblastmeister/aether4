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
     ((ty Int) (name main)
      (block
       ((Declare (ty Int) (var tmp@1))
        (Assign ((lvalue tmp@1) (expr (Int_const 0))))
        (Declare (ty Int) (var first@0))
        (Assign ((lvalue first@0) (expr (Var (var tmp@1) (ty ())))))
        (Declare (ty Int) (var tmp@3))
        (Assign ((lvalue tmp@3) (expr (Int_const 1234))))
        (Declare (ty Int) (var second@2))
        (Assign ((lvalue second@2) (expr (Var (var tmp@3) (ty ())))))
        (Declare (ty Bool) (var tmp@5))
        (Assign ((lvalue tmp@5) (expr (Bool_const true))))
        (Declare (ty Bool) (var third@4))
        (Assign ((lvalue third@4) (expr (Var (var tmp@5) (ty ())))))
        (Return
         (Bin (lhs (Var (var first@0) (ty ()))) (op Add)
          (rhs (Var (var second@2) (ty ()))) (ty ())))))))
    |}]
;;
