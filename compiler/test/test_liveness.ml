open Std
open Aether4
module Driver = Ae_driver
module C0 = Ae_c0_std
module Tir = Ae_tir_std
open Tir

let check s =
  let tir = Driver.compile_source_to_tir s |> Or_error.ok_exn in
  print_s [%message (tir : Func.t)];
  let pred_table = Func.pred_table tir in
  let live_in, live_out = Tir.Liveness.compute_non_ssa ~pred_table tir in
  print_s
    [%message (live_in : Tir.Liveness.Live_set.t) (live_out : Tir.Liveness.Live_set.t)];
  ()
;;

let check_next_use s =
  let tir = Driver.compile_source_to_tir s |> Or_error.ok_exn in
  let tir = Convert_ssa.convert ~renumber:() tir in
  print_s [%message (tir : Func.t)];
  let pred_table = Func.pred_table tir in
  let live_in, live_out = Liveness.compute_next_use_distance ~pred_table tir in
  print_s
    [%message
      (live_in : Liveness.Next_use_table.t) (live_out : Liveness.Next_use_table.t)];
  ()
;;

(* let%expect_test "simple next use backward transfer" =
  todo () *)

let simple_source =
  {|
  int main() {
      int first = 1342;
      int unused = 0;
      int second = 1234;
      int third = first + second;
      bool another = true;
      bool bl = false;
      if (another) {
        first += third;
      } else {
        another = bl;
      }
      return first + second;
  }
|}
;;

let source2 =
  {|
  int main() {
      int res;
      if (1 > 1) res = 1; else res = -1;
      return res;
  }
  |}
;;

let%expect_test "smoke" =
  check simple_source;
  [%expect
    {|
    (tir
     ((name main)
      (blocks
       ((join@0
         ((label join@0)
          (body
           (((i (Block_params (((param first@19) (ty Int))))) (index 0)
             (info ([9,7]-[13,8])) (ann ()))
            ((i (Unary (dst lhs@20) (op (Copy Int)) (src first@19))) (index 1)
             (info (3:11-16)) (ann ()))
            ((i (Unary (dst rhs@21) (op (Copy Int)) (src second@5))) (index 2)
             (info (5:11-17)) (ann ()))
            ((i (Bin (dst ret@22) (op Add) (src1 lhs@20) (src2 rhs@21)))
             (index 3) (info (14:14-28)) (ann ()))
            ((i (Ret (src ret@22) (ty Int))) (index 4) (info (14:7-28)) (ann ()))
            ((i Unreachable) (index 5) (info ([9,7]-[13,8])) (ann ()))))))
        (then@1
         ((label then@1)
          (body
           (((i (Block_params ())) (index 0) (info ([9,7]-[13,8])) (ann ()))
            ((i (Unary (dst lhs@16) (op (Copy Int)) (src first@1))) (index 1)
             (info (3:11-16)) (ann ()))
            ((i (Unary (dst rhs@17) (op (Copy Int)) (src third@9))) (index 2)
             (info (6:11-16)) (ann ()))
            ((i (Bin (dst first@18) (op Add) (src1 lhs@16) (src2 rhs@17)))
             (index 3) (info (10:9-23)) (ann ()))
            ((i (Jump ((label join@0) (args (first@18))))) (index 4)
             (info ([9,7]-[13,8])) (ann ()))
            ((i Unreachable) (index 5) (info ([9,7]-[13,8])) (ann ()))))))
        (else@2
         ((label else@2)
          (body
           (((i (Block_params ())) (index 0) (info ([9,7]-[13,8])) (ann ()))
            ((i (Unary (dst another@15) (op (Copy Bool)) (src bl@13))) (index 1)
             (info (8:12-14)) (ann ()))
            ((i (Jump ((label join@0) (args (first@1))))) (index 2)
             (info ([9,7]-[13,8])) (ann ()))
            ((i Unreachable) (index 3) (info ([9,7]-[13,8])) (ann ()))))))
        (start@3
         ((label start@3)
          (body
           (((i (Block_params ())) (index 0) (info ([2,3]-[15,4])) (ann ()))
            ((i (Nullary (dst tmp@0) (op (Int_const 1342)))) (index 1)
             (info (3:19-23)) (ann ()))
            ((i (Unary (dst first@1) (op (Copy Int)) (src tmp@0))) (index 2)
             (info (3:11-16)) (ann ()))
            ((i (Nullary (dst tmp@2) (op (Int_const 0)))) (index 3) (info (4:20))
             (ann ()))
            ((i (Unary (dst unused@3) (op (Copy Int)) (src tmp@2))) (index 4)
             (info (4:11-17)) (ann ()))
            ((i (Nullary (dst tmp@4) (op (Int_const 1234)))) (index 5)
             (info (5:20-24)) (ann ()))
            ((i (Unary (dst second@5) (op (Copy Int)) (src tmp@4))) (index 6)
             (info (5:11-17)) (ann ()))
            ((i (Unary (dst lhs@6) (op (Copy Int)) (src first@1))) (index 7)
             (info (3:11-16)) (ann ()))
            ((i (Unary (dst rhs@7) (op (Copy Int)) (src second@5))) (index 8)
             (info (5:11-17)) (ann ()))
            ((i (Bin (dst tmp@8) (op Add) (src1 lhs@6) (src2 rhs@7))) (index 9)
             (info (6:19-33)) (ann ()))
            ((i (Unary (dst third@9) (op (Copy Int)) (src tmp@8))) (index 10)
             (info (6:11-16)) (ann ()))
            ((i (Nullary (dst tmp@10) (op (Bool_const true)))) (index 11)
             (info (7:22-26)) (ann ()))
            ((i (Unary (dst another@11) (op (Copy Bool)) (src tmp@10)))
             (index 12) (info (7:12-19)) (ann ()))
            ((i (Nullary (dst tmp@12) (op (Bool_const false)))) (index 13)
             (info (8:17-22)) (ann ()))
            ((i (Unary (dst bl@13) (op (Copy Bool)) (src tmp@12))) (index 14)
             (info (8:12-14)) (ann ()))
            ((i (Unary (dst cond@14) (op (Copy Bool)) (src another@11)))
             (index 15) (info (7:12-19)) (ann ()))
            ((i
              (Cond_jump (cond cond@14) (b1 ((label then@1) (args ())))
               (b2 ((label else@2) (args ())))))
             (index 16) (info ([9,7]-[13,8])) (ann ()))
            ((i Unreachable) (index 17) (info ([2,3]-[15,4])) (ann ()))))))))
      (start start@3) (next_temp_id 23) (next_label_id 4)))
    ((live_in
      ((join@0 (second@5)) (then@1 (first@1 second@5 third@9))
       (else@2 (first@1 second@5 bl@13))))
     (live_out
      ((then@1 (second@5)) (else@2 (second@5))
       (start@3 (first@1 second@5 third@9 bl@13)))))
    |}]
;;

let%expect_test "smoke2" =
  check_next_use simple_source;
  [%expect
    {|
    (tir
     ((name main)
      (blocks
       ((join@0
         ((label join@0)
          (body
           (((i (Block_params (((param first@19) (ty Int))))) (index 0)
             (info ([9,7]-[13,8])) (ann ()))
            ((i (Unary (dst lhs@20) (op (Copy Int)) (src first@19))) (index 1)
             (info (3:11-16)) (ann ()))
            ((i (Unary (dst rhs@21) (op (Copy Int)) (src second@5))) (index 2)
             (info (5:11-17)) (ann ()))
            ((i (Bin (dst ret@22) (op Add) (src1 lhs@20) (src2 rhs@21)))
             (index 3) (info (14:14-28)) (ann ()))
            ((i (Ret (src ret@22) (ty Int))) (index 4) (info (14:7-28)) (ann ()))
            ((i Unreachable) (index 5) (info ([9,7]-[13,8])) (ann ()))))))
        (then@1
         ((label then@1)
          (body
           (((i (Block_params ())) (index 0) (info ([9,7]-[13,8])) (ann ()))
            ((i (Unary (dst lhs@16) (op (Copy Int)) (src first@1))) (index 1)
             (info (3:11-16)) (ann ()))
            ((i (Unary (dst rhs@17) (op (Copy Int)) (src third@9))) (index 2)
             (info (6:11-16)) (ann ()))
            ((i (Bin (dst first@18) (op Add) (src1 lhs@16) (src2 rhs@17)))
             (index 3) (info (10:9-23)) (ann ()))
            ((i (Jump ((label join@0) (args (first@18))))) (index 4)
             (info ([9,7]-[13,8])) (ann ()))
            ((i Unreachable) (index 5) (info ([9,7]-[13,8])) (ann ()))))))
        (else@2
         ((label else@2)
          (body
           (((i (Block_params ())) (index 0) (info ([9,7]-[13,8])) (ann ()))
            ((i (Unary (dst another@15) (op (Copy Bool)) (src bl@13))) (index 1)
             (info (8:12-14)) (ann ()))
            ((i (Jump ((label join@0) (args (first@1))))) (index 2)
             (info ([9,7]-[13,8])) (ann ()))
            ((i Unreachable) (index 3) (info ([9,7]-[13,8])) (ann ()))))))
        (start@3
         ((label start@3)
          (body
           (((i (Block_params ())) (index 0) (info ([2,3]-[15,4])) (ann ()))
            ((i (Nullary (dst tmp@0) (op (Int_const 1342)))) (index 1)
             (info (3:19-23)) (ann ()))
            ((i (Unary (dst first@1) (op (Copy Int)) (src tmp@0))) (index 2)
             (info (3:11-16)) (ann ()))
            ((i (Nullary (dst tmp@2) (op (Int_const 0)))) (index 3) (info (4:20))
             (ann ()))
            ((i (Unary (dst unused@3) (op (Copy Int)) (src tmp@2))) (index 4)
             (info (4:11-17)) (ann ()))
            ((i (Nullary (dst tmp@4) (op (Int_const 1234)))) (index 5)
             (info (5:20-24)) (ann ()))
            ((i (Unary (dst second@5) (op (Copy Int)) (src tmp@4))) (index 6)
             (info (5:11-17)) (ann ()))
            ((i (Unary (dst lhs@6) (op (Copy Int)) (src first@1))) (index 7)
             (info (3:11-16)) (ann ()))
            ((i (Unary (dst rhs@7) (op (Copy Int)) (src second@5))) (index 8)
             (info (5:11-17)) (ann ()))
            ((i (Bin (dst tmp@8) (op Add) (src1 lhs@6) (src2 rhs@7))) (index 9)
             (info (6:19-33)) (ann ()))
            ((i (Unary (dst third@9) (op (Copy Int)) (src tmp@8))) (index 10)
             (info (6:11-16)) (ann ()))
            ((i (Nullary (dst tmp@10) (op (Bool_const true)))) (index 11)
             (info (7:22-26)) (ann ()))
            ((i (Unary (dst another@11) (op (Copy Bool)) (src tmp@10)))
             (index 12) (info (7:12-19)) (ann ()))
            ((i (Nullary (dst tmp@12) (op (Bool_const false)))) (index 13)
             (info (8:17-22)) (ann ()))
            ((i (Unary (dst bl@13) (op (Copy Bool)) (src tmp@12))) (index 14)
             (info (8:12-14)) (ann ()))
            ((i (Unary (dst cond@14) (op (Copy Bool)) (src another@11)))
             (index 15) (info (7:12-19)) (ann ()))
            ((i
              (Cond_jump (cond cond@14) (b1 ((label then@1) (args ())))
               (b2 ((label else@2) (args ())))))
             (index 16) (info ([9,7]-[13,8])) (ann ()))
            ((i Unreachable) (index 17) (info ([2,3]-[15,4])) (ann ()))))))))
      (start start@3) (next_temp_id 23) (next_label_id 4)))
    ((live_in
      ((join@0 ((second@5 2))) (then@1 ((first@1 1) (second@5 8) (third@9 2)))
       (else@2 ((first@1 2) (second@5 6) (bl@13 1)))))
     (live_out
      ((then@1 ((second@5 2))) (else@2 ((second@5 2)))
       (start@3 ((first@1 1) (second@5 6) (third@9 2) (bl@13 1))))))
    |}]
;;

let%expect_test "smoke2" =
  check source2;
  [%expect
    {|
    (tir
     ((name main)
      (blocks
       ((join@0
         ((label join@0)
          (body
           (((i (Block_params (((param res@7) (ty Int))))) (index 0)
             (info (4:7-40)) (ann ()))
            ((i (Unary (dst ret@8) (op (Copy Int)) (src res@7))) (index 1)
             (info (3:11-14)) (ann ()))
            ((i (Ret (src ret@8) (ty Int))) (index 2) (info (5:7-17)) (ann ()))
            ((i Unreachable) (index 3) (info (4:7-40)) (ann ()))))))
        (then@1
         ((label then@1)
          (body
           (((i (Block_params ())) (index 0) (info (4:7-40)) (ann ()))
            ((i (Nullary (dst res@6) (op (Int_const 1)))) (index 1) (info (4:24))
             (ann ()))
            ((i (Jump ((label join@0) (args (res@6))))) (index 2) (info (4:7-40))
             (ann ()))
            ((i Unreachable) (index 3) (info (4:7-40)) (ann ()))))))
        (else@2
         ((label else@2)
          (body
           (((i (Block_params ())) (index 0) (info (4:7-40)) (ann ()))
            ((i (Nullary (dst lhs@3) (op (Int_const 0)))) (index 1)
             (info (4:38-40)) (ann ()))
            ((i (Nullary (dst rhs@4) (op (Int_const 1)))) (index 2) (info (4:39))
             (ann ()))
            ((i (Bin (dst res@5) (op Sub) (src1 lhs@3) (src2 rhs@4))) (index 3)
             (info (4:38-40)) (ann ()))
            ((i (Jump ((label join@0) (args (res@5))))) (index 4) (info (4:7-40))
             (ann ()))
            ((i Unreachable) (index 5) (info (4:7-40)) (ann ()))))))
        (start@3
         ((label start@3)
          (body
           (((i (Block_params ())) (index 0) (info ([2,3]-[6,4])) (ann ()))
            ((i (Nullary (dst lhs@0) (op (Int_const 1)))) (index 1) (info (4:11))
             (ann ()))
            ((i (Nullary (dst rhs@1) (op (Int_const 1)))) (index 2) (info (4:15))
             (ann ()))
            ((i (Bin (dst cond@2) (op Gt) (src1 lhs@0) (src2 rhs@1))) (index 3)
             (info (4:11-16)) (ann ()))
            ((i
              (Cond_jump (cond cond@2) (b1 ((label then@1) (args ())))
               (b2 ((label else@2) (args ())))))
             (index 4) (info (4:7-40)) (ann ()))
            ((i Unreachable) (index 5) (info ([2,3]-[6,4])) (ann ()))))))))
      (start start@3) (next_temp_id 9) (next_label_id 4)))
    ((live_in ()) (live_out ()))
    |}]
;;
