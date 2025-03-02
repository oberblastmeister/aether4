open Std
open Aether4
module Trace = Ae_trace
module Driver = Ae_driver
module C0 = Ae_c0_std
module Entity_graph_utils = Ae_entity_graph_utils
module Tir = Ae_tir_std
module Dominators = Ae_dominators
module Label = Ae_label_entity.Ident

let check s =
  let func = Driver.compile_source_to_tir s |> Or_error.ok_exn in
  let placements = Tir.Convert_ssa.compute_phi_placements func in
  print_s [%message (placements : Tir.Temp.t list Label.Table.t)];
  let func = Tir.Convert_ssa.convert func in
  Tir.Check_ssa.check func |> Or_error.ok_exn;
  let live_in, live_out =
    Tir.Liveness.compute ~pred_table:(Tir.Func.pred_table func) func
  in
  print_s
    [%message (live_in : Tir.Liveness.Live_set.t) (live_out : Tir.Liveness.Live_set.t)];
  print_s [%message (func : Tir.Func.t)];
  ()
;;

let%expect_test "smoke" =
  check
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
                second += 1;
              } else {
                another = bl;
                first += second;
              }
              bool final = another;
              return first + second;
          }
      |};
  [%expect
    {|
    (placements ((join@0 (another@7 second@4 first@3))))
    ((live_in
      ((then@1 (first@27 second@31 third@35 another@37))
       (else@2 (first@27 second@31 bl@39))))
     (live_out ((start@3 (first@27 second@31 third@35 another@37 bl@39)))))
    (func
     ((name main)
      (blocks
       ((join@0
         ((label join@0)
          (body
           (((i
              (Block_params
               (temps ((another@51 Bool) (second@52 Int) (first@53 Int)))))
             (index 0) (info ()))
            ((i (Unary (dst tmp@54) (op (Copy Bool)) (src another@51))) (index 1)
             (info ()))
            ((i (Unary (dst final@55) (op (Copy Bool)) (src tmp@54))) (index 2)
             (info ()))
            ((i (Unary (dst lhs@56) (op (Copy Int)) (src first@53))) (index 3)
             (info ()))
            ((i (Unary (dst rhs@57) (op (Copy Int)) (src second@52))) (index 4)
             (info ()))
            ((i (Bin (dst ret@58) (op Add) (src1 lhs@56) (src2 rhs@57)))
             (index 5) (info ()))
            ((i (Ret (src ret@58) (ty Int))) (index 6) (info ()))))))
        (then@1
         ((label then@1)
          (body
           (((i (Block_params (temps ()))) (index 0) (info ()))
            ((i (Unary (dst lhs@45) (op (Copy Int)) (src first@27))) (index 1)
             (info ()))
            ((i (Unary (dst rhs@46) (op (Copy Int)) (src third@35))) (index 2)
             (info ()))
            ((i (Bin (dst first@47) (op Add) (src1 lhs@45) (src2 rhs@46)))
             (index 3) (info ()))
            ((i (Unary (dst lhs@48) (op (Copy Int)) (src second@31))) (index 4)
             (info ()))
            ((i (Nullary (dst rhs@49) (op (Int_const 1)))) (index 5) (info ()))
            ((i (Bin (dst second@50) (op Add) (src1 lhs@48) (src2 rhs@49)))
             (index 6) (info ()))
            ((i (Jump ((label join@0) (args (another@37 second@50 first@47)))))
             (index 7) (info ()))))))
        (else@2
         ((label else@2)
          (body
           (((i (Block_params (temps ()))) (index 0) (info ()))
            ((i (Unary (dst another@41) (op (Copy Bool)) (src bl@39))) (index 1)
             (info ()))
            ((i (Unary (dst lhs@42) (op (Copy Int)) (src first@27))) (index 2)
             (info ()))
            ((i (Unary (dst rhs@43) (op (Copy Int)) (src second@31))) (index 3)
             (info ()))
            ((i (Bin (dst first@44) (op Add) (src1 lhs@42) (src2 rhs@43)))
             (index 4) (info ()))
            ((i (Jump ((label join@0) (args (another@41 second@31 first@44)))))
             (index 5) (info ()))))))
        (start@3
         ((label start@3)
          (body
           (((i (Block_params (temps ()))) (index 0) (info ()))
            ((i (Block_params (temps ()))) (index 1) (info ()))
            ((i (Nullary (dst tmp@26) (op (Int_const 1342)))) (index 2)
             (info ()))
            ((i (Unary (dst first@27) (op (Copy Int)) (src tmp@26))) (index 3)
             (info ()))
            ((i (Nullary (dst tmp@28) (op (Int_const 0)))) (index 4) (info ()))
            ((i (Unary (dst unused@29) (op (Copy Int)) (src tmp@28))) (index 5)
             (info ()))
            ((i (Nullary (dst tmp@30) (op (Int_const 1234)))) (index 6)
             (info ()))
            ((i (Unary (dst second@31) (op (Copy Int)) (src tmp@30))) (index 7)
             (info ()))
            ((i (Unary (dst lhs@32) (op (Copy Int)) (src first@27))) (index 8)
             (info ()))
            ((i (Unary (dst rhs@33) (op (Copy Int)) (src second@31))) (index 9)
             (info ()))
            ((i (Bin (dst tmp@34) (op Add) (src1 lhs@32) (src2 rhs@33)))
             (index 10) (info ()))
            ((i (Unary (dst third@35) (op (Copy Int)) (src tmp@34))) (index 11)
             (info ()))
            ((i (Nullary (dst tmp@36) (op (Bool_const true)))) (index 12)
             (info ()))
            ((i (Unary (dst another@37) (op (Copy Bool)) (src tmp@36)))
             (index 13) (info ()))
            ((i (Nullary (dst tmp@38) (op (Bool_const false)))) (index 14)
             (info ()))
            ((i (Unary (dst bl@39) (op (Copy Bool)) (src tmp@38))) (index 15)
             (info ()))
            ((i (Unary (dst cond@40) (op (Copy Bool)) (src another@37)))
             (index 16) (info ()))
            ((i
              (Cond_jump (cond cond@40) (b1 ((label then@1) (args ())))
               (b2 ((label else@2) (args ())))))
             (index 17) (info ()))))))))
      (start start@3) (next_temp_id 59) (next_label_id 4)))
    |}]
;;
