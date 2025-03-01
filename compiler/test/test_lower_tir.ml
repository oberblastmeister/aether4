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
      ((0
        ((key join@0)
         (data
          ((label join@0)
           (body
            (((i (Block_params (temps ()))) (index 0) (info ()))
             ((i (Unary (dst ret@0) (op (Copy Int)) (src first@1))) (index 1)
              (info ()))
             ((i (Ret (src ret@0) (ty Int))) (index 2) (info ()))))))))
       (1
        ((key then@1)
         (data
          ((label then@1)
           (body
            (((i (Block_params (temps ()))) (index 0) (info ()))
             ((i (Unary (dst lhs@4) (op (Copy Int)) (src first@1))) (index 1)
              (info ()))
             ((i (Unary (dst rhs@5) (op (Copy Int)) (src third@6))) (index 2)
              (info ()))
             ((i (Bin (dst first@1) (op Add) (src1 lhs@4) (src2 rhs@5)))
              (index 3) (info ()))
             ((i (Jump ((label join@0) (args ())))) (index 4) (info ()))))))))
       (2
        ((key else@2)
         (data
          ((label else@2)
           (body
            (((i (Block_params (temps ()))) (index 0) (info ()))
             ((i (Unary (dst another@3) (op (Copy Bool)) (src bl@7))) (index 1)
              (info ()))
             ((i (Jump ((label join@0) (args ())))) (index 2) (info ()))))))))
       (3
        ((key start@3)
         (data
          ((label start@3)
           (body
            (((i (Block_params (temps ()))) (index 0) (info ()))
             ((i (Block_params (temps ()))) (index 1) (info ()))
             ((i (Nullary (dst tmp@15) (op (Int_const 1342)))) (index 2)
              (info ()))
             ((i (Unary (dst first@1) (op (Copy Int)) (src tmp@15))) (index 3)
              (info ()))
             ((i (Nullary (dst tmp@14) (op (Int_const 1234)))) (index 4)
              (info ()))
             ((i (Unary (dst second@13) (op (Copy Int)) (src tmp@14))) (index 5)
              (info ()))
             ((i (Unary (dst lhs@11) (op (Copy Int)) (src first@1))) (index 6)
              (info ()))
             ((i (Unary (dst rhs@12) (op (Copy Int)) (src second@13))) (index 7)
              (info ()))
             ((i (Bin (dst tmp@10) (op Add) (src1 lhs@11) (src2 rhs@12)))
              (index 8) (info ()))
             ((i (Unary (dst third@6) (op (Copy Int)) (src tmp@10))) (index 9)
              (info ()))
             ((i (Nullary (dst tmp@9) (op (Bool_const true)))) (index 10)
              (info ()))
             ((i (Unary (dst another@3) (op (Copy Bool)) (src tmp@9))) (index 11)
              (info ()))
             ((i (Nullary (dst tmp@8) (op (Bool_const false)))) (index 12)
              (info ()))
             ((i (Unary (dst bl@7) (op (Copy Bool)) (src tmp@8))) (index 13)
              (info ()))
             ((i (Unary (dst cond@2) (op (Copy Bool)) (src another@3)))
              (index 14) (info ()))
             ((i
               (Cond_jump (cond cond@2) (b1 ((label then@1) (args ())))
                (b2 ((label else@2) (args ())))))
              (index 15) (info ()))))))))))
     (start start@3) (next_temp_id 16) (next_label_id 4))
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
  [%expect {|
    ((name main)
     (blocks
      ((0
        ((key join@0)
         (data
          ((label join@0)
           (body
            (((i (Block_params (temps ()))) (index 0) (info ()))
             ((i (Unary (dst ret@0) (op (Copy Int)) (src third@1))) (index 1)
              (info ()))
             ((i (Ret (src ret@0) (ty Int))) (index 2) (info ()))))))))
       (1
        ((key then@1)
         (data
          ((label then@1)
           (body
            (((i (Block_params (temps ()))) (index 0) (info ()))
             ((i (Nullary (dst first@4) (op (Int_const 1)))) (index 1) (info ()))
             ((i (Jump ((label join@0) (args ())))) (index 2) (info ()))))))))
       (2
        ((key else@2)
         (data
          ((label else@2)
           (body
            (((i (Block_params (temps ()))) (index 0) (info ()))
             ((i (Nullary (dst another@3) (op (Bool_const false)))) (index 1)
              (info ()))
             ((i (Jump ((label join@0) (args ())))) (index 2) (info ()))))))))
       (3
        ((key start@3)
         (data
          ((label start@3)
           (body
            (((i (Block_params (temps ()))) (index 0) (info ()))
             ((i (Block_params (temps ()))) (index 1) (info ()))
             ((i (Nullary (dst tmp@11) (op (Int_const 1342)))) (index 2)
              (info ()))
             ((i (Unary (dst first@4) (op (Copy Int)) (src tmp@11))) (index 3)
              (info ()))
             ((i (Nullary (dst tmp@10) (op (Int_const 1234)))) (index 4)
              (info ()))
             ((i (Unary (dst second@9) (op (Copy Int)) (src tmp@10))) (index 5)
              (info ()))
             ((i (Unary (dst lhs@7) (op (Copy Int)) (src first@4))) (index 6)
              (info ()))
             ((i (Unary (dst rhs@8) (op (Copy Int)) (src second@9))) (index 7)
              (info ()))
             ((i (Bin (dst tmp@6) (op Add) (src1 lhs@7) (src2 rhs@8))) (index 8)
              (info ()))
             ((i (Unary (dst third@1) (op (Copy Int)) (src tmp@6))) (index 9)
              (info ()))
             ((i (Nullary (dst tmp@5) (op (Bool_const true)))) (index 10)
              (info ()))
             ((i (Unary (dst another@3) (op (Copy Bool)) (src tmp@5))) (index 11)
              (info ()))
             ((i (Unary (dst cond@2) (op (Copy Bool)) (src another@3)))
              (index 12) (info ()))
             ((i
               (Cond_jump (cond cond@2) (b1 ((label then@1) (args ())))
                (b2 ((label else@2) (args ())))))
              (index 13) (info ()))))))))))
     (start start@3) (next_temp_id 12) (next_label_id 4))
    |}]
;;
