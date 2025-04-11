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
     ((Extern_func_defn (name _runtime_c0_panic@1)
       (ty ((ty (Void -1:-1)) (params ()) (span -1:-1))))
      (Func_decl (name main@0) (ty ((ty (Int -1:-1)) (params ()) (span -1:-1))))
      (Func_defn
       ((ty (Int 2:3-6)) (name main@0) (params ())
        (body
         ((Declare (ty (Int 3:13-16)) (var tmp@1) (span 3:13-26))
          (Assign (lvalue tmp@1) (expr (Int_const ((t 0) (span 3:25))))
           (span 3:13-26))
          (Declare (ty (Int 3:13-16)) (var first@0) (span 3:13-26))
          (Assign (lvalue first@0) (expr (Var (var tmp@1) (ty ())))
           (span 3:13-26))
          (Declare (ty (Int 4:13-16)) (var tmp@3) (span 4:13-30))
          (Assign (lvalue tmp@3) (expr (Int_const ((t 1234) (span 4:26-30))))
           (span 4:13-30))
          (Declare (ty (Int 4:13-16)) (var second@2) (span 4:13-30))
          (Assign (lvalue second@2) (expr (Var (var tmp@3) (ty ())))
           (span 4:13-30))
          (Declare (ty (Bool 5:13-17)) (var tmp@5) (span 5:13-30))
          (Assign (lvalue tmp@5) (expr (Bool_const ((t true) (span 5:26-30))))
           (span 5:13-30))
          (Declare (ty (Bool 5:13-17)) (var third@4) (span 5:13-30))
          (Assign (lvalue third@4) (expr (Var (var tmp@5) (ty ())))
           (span 5:13-30))
          (Return
           (expr
            (Bin (lhs (Var (var first@0) (ty ()))) (op Add)
             (rhs (Var (var second@2) (ty ()))) (ty ()) (span 6:20-34)))
           (span 6:13-34))))
        (span [2,3]-[7,12])))))
    |}]
;;
