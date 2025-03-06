open Std
open Aether4
module Driver = Ae_driver
module C0 = Ae_c0_std
module Tir = Ae_tir_std

let check s =
  let res = Driver.compile_source_to_tir s |> Or_error.ok_exn in
  print_s [%sexp (res : Tir.Func.t)];
  ()
;;

let%expect_test "smoke" =
  check
    {|
      int main() {
              int first = 1342;
              int second = 1234;
              int third = first + second;
              bool another = true;
              bool bl = false;
              if (another) {
                first += third;
              } else {
                another = bl;
              }
              return first;
          }
  |};
  [%expect
    {|
    ((name main)
     (blocks
      ((join@0
        ((label join@0)
         (body
          (((i (Block_params (temps ((first@33 Int))))) (index 0) (info ()))
           ((i (Unary (dst ret@34) (op (Copy Int)) (src first@33))) (index 1)
            (info ()))
           ((i (Ret (src ret@34) (ty Int))) (index 2) (info ()))))))
       (then@1
        ((label then@1)
         (body
          (((i (Block_params (temps ()))) (index 0) (info ()))
           ((i (Unary (dst lhs@30) (op (Copy Int)) (src first@17))) (index 1)
            (info ()))
           ((i (Unary (dst rhs@31) (op (Copy Int)) (src third@23))) (index 2)
            (info ()))
           ((i (Bin (dst first@32) (op Add) (src1 lhs@30) (src2 rhs@31)))
            (index 3) (info ()))
           ((i (Jump ((label join@0) (args (first@32))))) (index 4) (info ()))))))
       (else@2
        ((label else@2)
         (body
          (((i (Block_params (temps ()))) (index 0) (info ()))
           ((i (Unary (dst another@29) (op (Copy Bool)) (src bl@27))) (index 1)
            (info ()))
           ((i (Jump ((label join@0) (args (first@17))))) (index 2) (info ()))))))
       (start@3
        ((label start@3)
         (body
          (((i (Block_params (temps ()))) (index 0) (info ()))
           ((i (Nullary (dst tmp@16) (op (Int_const 1342)))) (index 1) (info ()))
           ((i (Unary (dst first@17) (op (Copy Int)) (src tmp@16))) (index 2)
            (info ()))
           ((i (Nullary (dst tmp@18) (op (Int_const 1234)))) (index 3) (info ()))
           ((i (Unary (dst second@19) (op (Copy Int)) (src tmp@18))) (index 4)
            (info ()))
           ((i (Unary (dst lhs@20) (op (Copy Int)) (src first@17))) (index 5)
            (info ()))
           ((i (Unary (dst rhs@21) (op (Copy Int)) (src second@19))) (index 6)
            (info ()))
           ((i (Bin (dst tmp@22) (op Add) (src1 lhs@20) (src2 rhs@21))) (index 7)
            (info ()))
           ((i (Unary (dst third@23) (op (Copy Int)) (src tmp@22))) (index 8)
            (info ()))
           ((i (Nullary (dst tmp@24) (op (Bool_const true)))) (index 9)
            (info ()))
           ((i (Unary (dst another@25) (op (Copy Bool)) (src tmp@24))) (index 10)
            (info ()))
           ((i (Nullary (dst tmp@26) (op (Bool_const false)))) (index 11)
            (info ()))
           ((i (Unary (dst bl@27) (op (Copy Bool)) (src tmp@26))) (index 12)
            (info ()))
           ((i (Unary (dst cond@28) (op (Copy Bool)) (src another@25)))
            (index 13) (info ()))
           ((i
             (Cond_jump (cond cond@28) (b1 ((label then@1) (args ())))
              (b2 ((label else@2) (args ())))))
            (index 14) (info ()))))))))
     (start start@3) (next_temp_id 35) (next_label_id 4))
    |}]
;;

let%expect_test "while" =
  check
    {|
    int main() {
      int first = 1342;
      int second = 1234;
      int third = first + second;
      bool another = true;
      if (another) {
        first = 1;
      } else {
        another = false;
      }
      return third;
    }
  |};
  [%expect
    {|
    ((name main)
     (blocks
      ((join@0
        ((label join@0)
         (body
          (((i (Block_params (temps ()))) (index 0) (info ()))
           ((i (Unary (dst ret@25) (op (Copy Int)) (src third@19))) (index 1)
            (info ()))
           ((i (Ret (src ret@25) (ty Int))) (index 2) (info ()))))))
       (then@1
        ((label then@1)
         (body
          (((i (Block_params (temps ()))) (index 0) (info ()))
           ((i (Nullary (dst first@24) (op (Int_const 1)))) (index 1) (info ()))
           ((i (Jump ((label join@0) (args ())))) (index 2) (info ()))))))
       (else@2
        ((label else@2)
         (body
          (((i (Block_params (temps ()))) (index 0) (info ()))
           ((i (Nullary (dst another@23) (op (Bool_const false)))) (index 1)
            (info ()))
           ((i (Jump ((label join@0) (args ())))) (index 2) (info ()))))))
       (start@3
        ((label start@3)
         (body
          (((i (Block_params (temps ()))) (index 0) (info ()))
           ((i (Nullary (dst tmp@12) (op (Int_const 1342)))) (index 1) (info ()))
           ((i (Unary (dst first@13) (op (Copy Int)) (src tmp@12))) (index 2)
            (info ()))
           ((i (Nullary (dst tmp@14) (op (Int_const 1234)))) (index 3) (info ()))
           ((i (Unary (dst second@15) (op (Copy Int)) (src tmp@14))) (index 4)
            (info ()))
           ((i (Unary (dst lhs@16) (op (Copy Int)) (src first@13))) (index 5)
            (info ()))
           ((i (Unary (dst rhs@17) (op (Copy Int)) (src second@15))) (index 6)
            (info ()))
           ((i (Bin (dst tmp@18) (op Add) (src1 lhs@16) (src2 rhs@17))) (index 7)
            (info ()))
           ((i (Unary (dst third@19) (op (Copy Int)) (src tmp@18))) (index 8)
            (info ()))
           ((i (Nullary (dst tmp@20) (op (Bool_const true)))) (index 9)
            (info ()))
           ((i (Unary (dst another@21) (op (Copy Bool)) (src tmp@20))) (index 10)
            (info ()))
           ((i (Unary (dst cond@22) (op (Copy Bool)) (src another@21)))
            (index 11) (info ()))
           ((i
             (Cond_jump (cond cond@22) (b1 ((label then@1) (args ())))
              (b2 ((label else@2) (args ())))))
            (index 12) (info ()))))))))
     (start start@3) (next_temp_id 26) (next_label_id 4))
    |}]
;;
