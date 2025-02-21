open Std
module Stack_builder = Ae_stack_builder
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

(* let%expect_test "smoke" =
  check
    {|
  int main() {
    int first = 01234;
    first += 1234;
    
    int another = 12;
  }
 |};
  [%expect
    {|
    (Ok
     ((ty Int) (name main)
      (block
       ((Declare (ty Int) (var tmp@1))
        (Assign (lvalue tmp@1) (expr (IntConst 1234)))
        (Declare (ty Int) (var first@0))
        (Assign (lvalue first@0) (expr (Var tmp@1)))
        (Assign (lvalue first@0)
         (expr (Bin (lhs (Var first@0)) (op Add) (rhs (IntConst 1234)))))
        (Declare (ty Int) (var tmp@3))
        (Assign (lvalue tmp@3) (expr (IntConst 12)))
        (Declare (ty Int) (var another@2))
        (Assign (lvalue another@2) (expr (Var tmp@3)))))))
    |}]
;; *)
