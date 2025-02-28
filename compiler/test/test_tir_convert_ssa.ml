open Std
open Aether4
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
  let func = Ae_tir_convert_ssa.convert func in
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
              return first + second;
          }
      |};
  [%expect
    {|
    (placements ((join@0 (second@4 first@3))))
    (func
     ((name main)
      (blocks
       ((0
         ((key join@0)
          (data
           ((label join@0)
            (body
             (((i (BlockParams (temps ((second@49 Int) (first@50 Int)))))
               (index 0) (info ()))
              ((i (Unary (dst lhs@51) (op (Copy Int)) (src first@50))) (index 2)
               (info ()))
              ((i (Unary (dst rhs@52) (op (Copy Int)) (src second@49))) (index 4)
               (info ()))
              ((i (Bin (dst ret@53) (op Add) (src1 lhs@51) (src2 rhs@52)))
               (index 6) (info ()))
              ((i (Ret (src ret@53) (ty Int))) (index 8) (info ()))))))))
        (1
         ((key then@1)
          (data
           ((label then@1)
            (body
             (((i (BlockParams (temps ()))) (index 0) (info ()))
              ((i (Unary (dst lhs@43) (op (Copy Int)) (src first@25))) (index 2)
               (info ()))
              ((i (Unary (dst rhs@44) (op (Copy Int)) (src third@33))) (index 4)
               (info ()))
              ((i (Bin (dst first@45) (op Add) (src1 lhs@43) (src2 rhs@44)))
               (index 6) (info ()))
              ((i (Unary (dst lhs@46) (op (Copy Int)) (src second@29))) (index 8)
               (info ()))
              ((i (Nullary (dst rhs@47) (op (IntConst 1)))) (index 10) (info ()))
              ((i (Bin (dst second@48) (op Add) (src1 lhs@46) (src2 rhs@47)))
               (index 12) (info ()))
              ((i (Jump ((label join@0) (args (second@48 first@45))))) (index 14)
               (info ()))))))))
        (2
         ((key else@2)
          (data
           ((label else@2)
            (body
             (((i (BlockParams (temps ()))) (index 0) (info ()))
              ((i (Unary (dst another@39) (op (Copy Bool)) (src bl@37)))
               (index 2) (info ()))
              ((i (Unary (dst lhs@40) (op (Copy Int)) (src first@25))) (index 4)
               (info ()))
              ((i (Unary (dst rhs@41) (op (Copy Int)) (src second@29))) (index 6)
               (info ()))
              ((i (Bin (dst first@42) (op Add) (src1 lhs@40) (src2 rhs@41)))
               (index 8) (info ()))
              ((i (Jump ((label join@0) (args (second@29 first@42))))) (index 10)
               (info ()))))))))
        (3
         ((key start@3)
          (data
           ((label start@3)
            (body
             (((i (BlockParams (temps ()))) (index 0) (info ()))
              ((i (BlockParams (temps ()))) (index 2) (info ()))
              ((i (Nullary (dst tmp@24) (op (IntConst 1342)))) (index 4)
               (info ()))
              ((i (Unary (dst first@25) (op (Copy Int)) (src tmp@24))) (index 6)
               (info ()))
              ((i (Nullary (dst tmp@26) (op (IntConst 0)))) (index 8) (info ()))
              ((i (Unary (dst unused@27) (op (Copy Int)) (src tmp@26)))
               (index 10) (info ()))
              ((i (Nullary (dst tmp@28) (op (IntConst 1234)))) (index 12)
               (info ()))
              ((i (Unary (dst second@29) (op (Copy Int)) (src tmp@28)))
               (index 14) (info ()))
              ((i (Unary (dst lhs@30) (op (Copy Int)) (src first@25))) (index 16)
               (info ()))
              ((i (Unary (dst rhs@31) (op (Copy Int)) (src second@29)))
               (index 18) (info ()))
              ((i (Bin (dst tmp@32) (op Add) (src1 lhs@30) (src2 rhs@31)))
               (index 20) (info ()))
              ((i (Unary (dst third@33) (op (Copy Int)) (src tmp@32))) (index 22)
               (info ()))
              ((i (Nullary (dst tmp@34) (op (BoolConst true)))) (index 24)
               (info ()))
              ((i (Unary (dst another@35) (op (Copy Bool)) (src tmp@34)))
               (index 26) (info ()))
              ((i (Nullary (dst tmp@36) (op (BoolConst false)))) (index 28)
               (info ()))
              ((i (Unary (dst bl@37) (op (Copy Bool)) (src tmp@36))) (index 30)
               (info ()))
              ((i (Unary (dst cond@38) (op (Copy Bool)) (src another@35)))
               (index 32) (info ()))
              ((i
                (CondJump (cond cond@38) (b1 ((label then@1) (args ())))
                 (b2 ((label else@2) (args ())))))
               (index 34) (info ()))))))))))
      (start start@3) (next_temp_id 54) (next_label_id 4)))
    |}]
;;
