open Std
open Aether4
module Driver = Ae_driver
module C0 = Ae_c0_std
module Tir = Ae_tir_std

let check s =
  let res = Driver.compile_source_to_tir s |> Or_error.ok_exn in
  print_s [%sexp (res : Tir.Program.t)];
  ()
;;

let%expect_test "smoke" =
  check
    {|
      void main() {
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
          }
  |};
  [%expect
    {|
    ((funcs
      (((name main)
        (blocks
         ((start@0
           ((label start@0)
            (body
             (((i (Block_params ())) (index 0) (info ()) (ann ()))
              ((i (Nullary (dst tmp@0) (op (Int_const 1342)))) (index 1)
               (info (3:27-31)) (ann ()))
              ((i (Unary (dst first@1) (op (Copy Int)) (src tmp@0))) (index 2)
               (info (3:19-24)) (ann ()))
              ((i (Nullary (dst tmp@2) (op (Int_const 1234)))) (index 3)
               (info (4:28-32)) (ann ()))
              ((i (Unary (dst second@3) (op (Copy Int)) (src tmp@2))) (index 4)
               (info (4:19-25)) (ann ()))
              ((i (Unary (dst lhs@4) (op (Copy Int)) (src first@1))) (index 5)
               (info (3:19-24)) (ann ()))
              ((i (Unary (dst rhs@5) (op (Copy Int)) (src second@3))) (index 6)
               (info (4:19-25)) (ann ()))
              ((i (Bin (dst tmp@6) (op Add) (src1 lhs@4) (src2 rhs@5))) (index 7)
               (info (5:27-41)) (ann ()))
              ((i (Unary (dst third@7) (op (Copy Int)) (src tmp@6))) (index 8)
               (info (5:19-24)) (ann ()))
              ((i (Nullary (dst tmp@8) (op (Bool_const true)))) (index 9)
               (info (6:30-34)) (ann ()))
              ((i (Unary (dst another@9) (op (Copy Bool)) (src tmp@8)))
               (index 10) (info (6:20-27)) (ann ()))
              ((i (Nullary (dst tmp@10) (op (Bool_const false)))) (index 11)
               (info (7:25-30)) (ann ()))
              ((i (Unary (dst bl@11) (op (Copy Bool)) (src tmp@10))) (index 12)
               (info (7:20-22)) (ann ()))
              ((i (Unary (dst cond@12) (op (Copy Bool)) (src another@9)))
               (index 13) (info (6:20-27)) (ann ()))
              ((i
                (Cond_jump (cond cond@12) (b1 ((label then@1) (args ())))
                 (b2 ((label else@2) (args ())))))
               (index 14) (info ()) (ann ()))))))
          (then@1
           ((label then@1)
            (body
             (((i (Block_params ())) (index 0) (info ()) (ann ()))
              ((i (Unary (dst lhs@15) (op (Copy Int)) (src first@1))) (index 1)
               (info (3:19-24)) (ann ()))
              ((i (Unary (dst rhs@16) (op (Copy Int)) (src third@7))) (index 2)
               (info (5:19-24)) (ann ()))
              ((i (Bin (dst first@17) (op Add) (src1 lhs@15) (src2 rhs@16)))
               (index 3) (info (9:17-31)) (ann ()))
              ((i (Jump ((label join@3) (args ())))) (index 4) (info ())
               (ann ()))))))
          (else@2
           ((label else@2)
            (body
             (((i (Block_params ())) (index 0) (info ()) (ann ()))
              ((i (Unary (dst another@14) (op (Copy Bool)) (src bl@11)))
               (index 1) (info (7:20-22)) (ann ()))
              ((i (Jump ((label join@3) (args ())))) (index 2) (info ())
               (ann ()))))))
          (join@3
           ((label join@3)
            (body
             (((i (Block_params ())) (index 0) (info ()) (ann ()))
              ((i (Nullary (dst ret@13) (op Void_const))) (index 1)
               (info ([2,7]-[13,12])) (ann ()))
              ((i (Ret (src ret@13) (ty Void))) (index 2) (info ([2,7]-[13,12]))
               (ann ()))))))))
        (start start@0) (next_temp_id 18) (next_label_id 4)))))
    |}]
;;

let%expect_test "while" =
  check
    {|
    void main() {
      int first = 1342;
      int second = 1234;
      int third = first + second;
      bool another = true;
      if (another) {
        first = 1;
      } else {
        another = false;
      }
    }
  |};
  [%expect
    {|
    ((funcs
      (((name main)
        (blocks
         ((start@0
           ((label start@0)
            (body
             (((i (Block_params ())) (index 0) (info ()) (ann ()))
              ((i (Nullary (dst tmp@0) (op (Int_const 1342)))) (index 1)
               (info (3:19-23)) (ann ()))
              ((i (Unary (dst first@1) (op (Copy Int)) (src tmp@0))) (index 2)
               (info (3:11-16)) (ann ()))
              ((i (Nullary (dst tmp@2) (op (Int_const 1234)))) (index 3)
               (info (4:20-24)) (ann ()))
              ((i (Unary (dst second@3) (op (Copy Int)) (src tmp@2))) (index 4)
               (info (4:11-17)) (ann ()))
              ((i (Unary (dst lhs@4) (op (Copy Int)) (src first@1))) (index 5)
               (info (3:11-16)) (ann ()))
              ((i (Unary (dst rhs@5) (op (Copy Int)) (src second@3))) (index 6)
               (info (4:11-17)) (ann ()))
              ((i (Bin (dst tmp@6) (op Add) (src1 lhs@4) (src2 rhs@5))) (index 7)
               (info (5:19-33)) (ann ()))
              ((i (Unary (dst third@7) (op (Copy Int)) (src tmp@6))) (index 8)
               (info (5:11-16)) (ann ()))
              ((i (Nullary (dst tmp@8) (op (Bool_const true)))) (index 9)
               (info (6:22-26)) (ann ()))
              ((i (Unary (dst another@9) (op (Copy Bool)) (src tmp@8)))
               (index 10) (info (6:12-19)) (ann ()))
              ((i (Unary (dst cond@10) (op (Copy Bool)) (src another@9)))
               (index 11) (info (6:12-19)) (ann ()))
              ((i
                (Cond_jump (cond cond@10) (b1 ((label then@1) (args ())))
                 (b2 ((label else@2) (args ())))))
               (index 12) (info ()) (ann ()))))))
          (then@1
           ((label then@1)
            (body
             (((i (Block_params ())) (index 0) (info ()) (ann ()))
              ((i (Nullary (dst first@13) (op (Int_const 1)))) (index 1)
               (info (8:17)) (ann ()))
              ((i (Jump ((label join@3) (args ())))) (index 2) (info ())
               (ann ()))))))
          (else@2
           ((label else@2)
            (body
             (((i (Block_params ())) (index 0) (info ()) (ann ()))
              ((i (Nullary (dst another@12) (op (Bool_const false)))) (index 1)
               (info (10:19-24)) (ann ()))
              ((i (Jump ((label join@3) (args ())))) (index 2) (info ())
               (ann ()))))))
          (join@3
           ((label join@3)
            (body
             (((i (Block_params ())) (index 0) (info ()) (ann ()))
              ((i (Nullary (dst ret@11) (op Void_const))) (index 1)
               (info ([2,5]-[12,6])) (ann ()))
              ((i (Ret (src ret@11) (ty Void))) (index 2) (info ([2,5]-[12,6]))
               (ann ()))))))))
        (start start@0) (next_temp_id 14) (next_label_id 4)))))
    |}]
;;
