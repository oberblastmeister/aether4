open Std
open Aether4
module Driver = Ae_driver
module C0 = Ae_c0_std
module Tir = Ae_tir_std
open Tir

let check s =
  let tir =
    Driver.compile_source_to_tir s |> Or_error.ok_exn |> Program.funcs |> List.hd_exn
  in
  print_s [%message (tir : Func.t)];
  let pred_table = Func.pred_table tir in
  let live_in, live_out = Tir.Liveness.compute_non_ssa ~pred_table tir in
  print_s
    [%message (live_in : Tir.Liveness.Live_set.t) (live_out : Tir.Liveness.Live_set.t)];
  ()
;;

let check_next_use s =
  let tir =
    Driver.compile_source_to_tir s |> Or_error.ok_exn |> Program.funcs |> List.hd_exn
  in
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
  void main() {
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
      int res = first + second;
  }
|}
;;

let source2 =
  {|
  void main() {
      int res;
      if (1 > 1) res = 1; else res = -1;
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
       ((start@0
         ((label start@0)
          (body
           (((i (Block_params ())) (index 0) (info ()) (ann ()))
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
             (index 16) (info ()) (ann ()))))))
        (then@1
         ((label then@1)
          (body
           (((i (Block_params ())) (index 0) (info ()) (ann ()))
            ((i (Unary (dst lhs@22) (op (Copy Int)) (src first@1))) (index 1)
             (info (3:11-16)) (ann ()))
            ((i (Unary (dst rhs@23) (op (Copy Int)) (src third@9))) (index 2)
             (info (6:11-16)) (ann ()))
            ((i (Bin (dst first@24) (op Add) (src1 lhs@22) (src2 rhs@23)))
             (index 3) (info (10:9-23)) (ann ()))
            ((i (Jump ((label join@3) (args (first@24))))) (index 4) (info ())
             (ann ()))))))
        (else@2
         ((label else@2)
          (body
           (((i (Block_params ())) (index 0) (info ()) (ann ()))
            ((i (Unary (dst another@21) (op (Copy Bool)) (src bl@13))) (index 1)
             (info (8:12-14)) (ann ()))
            ((i (Jump ((label join@3) (args (first@1))))) (index 2) (info ())
             (ann ()))))))
        (join@3
         ((label join@3)
          (body
           (((i (Block_params (((param first@15) (ty Int))))) (index 0) (info ())
             (ann ()))
            ((i (Unary (dst lhs@16) (op (Copy Int)) (src first@15))) (index 1)
             (info (3:11-16)) (ann ()))
            ((i (Unary (dst rhs@17) (op (Copy Int)) (src second@5))) (index 2)
             (info (5:11-17)) (ann ()))
            ((i (Bin (dst tmp@18) (op Add) (src1 lhs@16) (src2 rhs@17)))
             (index 3) (info (14:17-31)) (ann ()))
            ((i (Unary (dst res@19) (op (Copy Int)) (src tmp@18))) (index 4)
             (info (14:11-14)) (ann ()))
            ((i (Nullary (dst ret@20) (op Void_const))) (index 5)
             (info ([2,3]-[15,4])) (ann ()))
            ((i (Ret (src ret@20) (ty Void))) (index 6) (info ([2,3]-[15,4]))
             (ann ()))))))))
      (start start@0) (next_temp_id 25) (next_label_id 4)))
    ((live_in
      ((then@1 (first@1 second@5 third@9)) (else@2 (first@1 second@5 bl@13))
       (join@3 (second@5))))
     (live_out
      ((start@0 (first@1 second@5 third@9 bl@13)) (then@1 (second@5))
       (else@2 (second@5)))))
    |}]
;;

let%expect_test "smoke2" =
  check_next_use simple_source;
  [%expect
    {|
    (tir
     ((name main)
      (blocks
       ((start@0
         ((label start@0)
          (body
           (((i (Block_params ())) (index 0) (info ()) (ann ()))
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
             (index 16) (info ()) (ann ()))))))
        (then@1
         ((label then@1)
          (body
           (((i (Block_params ())) (index 0) (info ()) (ann ()))
            ((i (Unary (dst lhs@22) (op (Copy Int)) (src first@1))) (index 1)
             (info (3:11-16)) (ann ()))
            ((i (Unary (dst rhs@23) (op (Copy Int)) (src third@9))) (index 2)
             (info (6:11-16)) (ann ()))
            ((i (Bin (dst first@24) (op Add) (src1 lhs@22) (src2 rhs@23)))
             (index 3) (info (10:9-23)) (ann ()))
            ((i (Jump ((label join@3) (args (first@24))))) (index 4) (info ())
             (ann ()))))))
        (else@2
         ((label else@2)
          (body
           (((i (Block_params ())) (index 0) (info ()) (ann ()))
            ((i (Unary (dst another@21) (op (Copy Bool)) (src bl@13))) (index 1)
             (info (8:12-14)) (ann ()))
            ((i (Jump ((label join@3) (args (first@1))))) (index 2) (info ())
             (ann ()))))))
        (join@3
         ((label join@3)
          (body
           (((i (Block_params (((param first@15) (ty Int))))) (index 0) (info ())
             (ann ()))
            ((i (Unary (dst lhs@16) (op (Copy Int)) (src first@15))) (index 1)
             (info (3:11-16)) (ann ()))
            ((i (Unary (dst rhs@17) (op (Copy Int)) (src second@5))) (index 2)
             (info (5:11-17)) (ann ()))
            ((i (Bin (dst tmp@18) (op Add) (src1 lhs@16) (src2 rhs@17)))
             (index 3) (info (14:17-31)) (ann ()))
            ((i (Unary (dst res@19) (op (Copy Int)) (src tmp@18))) (index 4)
             (info (14:11-14)) (ann ()))
            ((i (Nullary (dst ret@20) (op Void_const))) (index 5)
             (info ([2,3]-[15,4])) (ann ()))
            ((i (Ret (src ret@20) (ty Void))) (index 6) (info ([2,3]-[15,4]))
             (ann ()))))))))
      (start start@0) (next_temp_id 25) (next_label_id 4)))
    ((live_in
      ((then@1 ((first@1 1) (second@5 7) (third@9 2)))
       (else@2 ((first@1 2) (second@5 5) (bl@13 1))) (join@3 ((second@5 2)))))
     (live_out
      ((start@0 ((first@1 1) (second@5 5) (third@9 2) (bl@13 1)))
       (then@1 ((second@5 2))) (else@2 ((second@5 2))))))
    |}]
;;

let%expect_test "smoke2" =
  check source2;
  [%expect
    {|
    (tir
     ((name main)
      (blocks
       ((start@0
         ((label start@0)
          (body
           (((i (Block_params ())) (index 0) (info ()) (ann ()))
            ((i (Nullary (dst lhs@0) (op (Int_const 1)))) (index 1) (info (4:11))
             (ann ()))
            ((i (Nullary (dst rhs@1) (op (Int_const 1)))) (index 2) (info (4:15))
             (ann ()))
            ((i (Bin (dst cond@2) (op Gt) (src1 lhs@0) (src2 rhs@1))) (index 3)
             (info (4:11-16)) (ann ()))
            ((i
              (Cond_jump (cond cond@2) (b1 ((label then@1) (args ())))
               (b2 ((label else@2) (args ())))))
             (index 4) (info ()) (ann ()))))))
        (then@1
         ((label then@1)
          (body
           (((i (Block_params ())) (index 0) (info ()) (ann ()))
            ((i (Nullary (dst res@7) (op (Int_const 1)))) (index 1) (info (4:24))
             (ann ()))
            ((i (Jump ((label join@3) (args ())))) (index 2) (info ()) (ann ()))))))
        (else@2
         ((label else@2)
          (body
           (((i (Block_params ())) (index 0) (info ()) (ann ()))
            ((i (Nullary (dst lhs@4) (op (Int_const 0)))) (index 1)
             (info (4:38-40)) (ann ()))
            ((i (Nullary (dst rhs@5) (op (Int_const 1)))) (index 2) (info (4:39))
             (ann ()))
            ((i (Bin (dst res@6) (op Sub) (src1 lhs@4) (src2 rhs@5))) (index 3)
             (info (4:38-40)) (ann ()))
            ((i (Jump ((label join@3) (args ())))) (index 4) (info ()) (ann ()))))))
        (join@3
         ((label join@3)
          (body
           (((i (Block_params ())) (index 0) (info ()) (ann ()))
            ((i (Nullary (dst ret@3) (op Void_const))) (index 1)
             (info ([2,3]-[5,4])) (ann ()))
            ((i (Ret (src ret@3) (ty Void))) (index 2) (info ([2,3]-[5,4]))
             (ann ()))))))))
      (start start@0) (next_temp_id 8) (next_label_id 4)))
    ((live_in ()) (live_out ()))
    |}]
;;
