open Std
module C0 = Ae_c0_std
module Tir = Ae_tir_std
module Lir = Ae_lir_std
module Abs_x86 = Ae_abs_x86_std

let check s =
  let tokens = C0.Lexer.tokenize s in
  let program = C0.Parser.parse tokens |> Result.ok |> Option.value_exn in
  let tir = C0.Lower_tree_ir.lower program in
  let lir = Tir.Lower_lir.lower tir in
  let abs_x86 = Lir.Lower_abs_x86.lower lir in
  let alloc = Abs_x86.Regalloc.alloc_func abs_x86 in
  print_s [%sexp (abs_x86 : Abs_x86.Func.t)];
  print_s [%sexp (alloc : Abs_x86.Alloc_reg.t Abs_x86.Vreg.Table.t)];
  ()
;;

let%expect_test "simple" =
  check
    {|
      int first() {
        int first = 12 + 1234 - 12;
        int second = first + 12;
        second += first + second;
      }
    |};
  [%expect
    {|
    ((name first)
     (blocks
      ((0
        ((key start@0)
         (data
          ((body
            ((BlockMov (temps ())) (Mov (dst (Reg lhs@3)) (src (Imm 12)))
             (Mov (dst (Reg rhs@4)) (src (Imm 1234)))
             (Bin (dst (Reg lhs@1)) (op Add) (src1 (Reg lhs@3))
              (src2 (Reg rhs@4)))
             (Mov (dst (Reg rhs@2)) (src (Imm 12)))
             (Bin (dst (Reg first@0)) (op Sub) (src1 (Reg lhs@1))
              (src2 (Reg rhs@2)))
             (Mov (dst (Reg lhs@6)) (src (Reg first@0)))
             (Mov (dst (Reg rhs@7)) (src (Imm 12)))
             (Bin (dst (Reg second@5)) (op Add) (src1 (Reg lhs@6))
              (src2 (Reg rhs@7)))
             (Mov (dst (Reg lhs@8)) (src (Reg second@5)))
             (Mov (dst (Reg lhs@10)) (src (Reg first@0)))
             (Mov (dst (Reg rhs@11)) (src (Reg second@5)))
             (Bin (dst (Reg rhs@9)) (op Add) (src1 (Reg lhs@10))
              (src2 (Reg rhs@11)))
             (Bin (dst (Reg second@5)) (op Add) (src1 (Reg lhs@8))
              (src2 (Reg rhs@9)))
             (Mov (dst (Reg ret@12)) (src (Imm 0))) (Ret (src (Reg ret@12)))))))))))
     (start start@0) (next_id 13))
    ((first@0 (InReg RDI)) (lhs@1 (InReg RDI)) (rhs@2 (InReg RAX))
     (lhs@3 (InReg RDI)) (rhs@4 (InReg RAX)) (second@5 (InReg RAX))
     (lhs@6 (InReg RDI)) (rhs@7 (InReg RAX)) (lhs@8 (InReg RSI))
     (rhs@9 (InReg RAX)) (lhs@10 (InReg RDI)) (rhs@11 (InReg RAX))
     (ret@12 (InReg RAX)))
    |}]
;;
