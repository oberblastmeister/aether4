open Std
module C0 = Ae_c0_std
module Tir = Ae_tir_std
module Lir = Ae_lir_lower_abs_x86
module Abs_x86 = Ae_abs_x86_std
module Flat_x86 = Ae_flat_x86_std

let check s =
  let open struct end in
  let tokens = C0.Lexer.tokenize s in
  let program = C0.Parser.parse tokens |> Result.ok |> Option.value_exn in
  let tir = C0.Lower_tree_ir.lower program in
  let lir = Tir.Lower_lir.lower tir in
  let abs_x86 = Lir.lower lir in
  let alloc = Abs_x86.Regalloc.alloc_func abs_x86 in
  let asm = Abs_x86.Lower_flat_x86.lower alloc abs_x86 in
  let formatted_asm = Flat_x86.Format.format asm in
  print_endline formatted_asm;
  ()
;;

let%expect_test "nothing" =
  check
    {|
    int first() {
    
    }
  |};
  [%expect
    {|
    .text
    .globl _c0_main
    _c0_main:
    pushq %rbp
    movq %rsp, %rbp
    subq $0, %rsp
    movq $0, %rax
    movq %rax, %rax
    movq %rbp, %rsp
    popq %rbp
    ret
    |}]
;;

let%expect_test "add const" =
  check
    {|
    int first() {
      int first = 1234 + 123;
    }
  |};
  [%expect
    {|
    .text
    .globl _c0_main
    _c0_main:
    pushq %rbp
    movq %rsp, %rbp
    subq $0, %rsp
    movq $1234, %rdi
    movq $123, %rax
    movq %rdi, %r11
    addq %rax, %r11
    movq %r11, %rax
    movq $0, %rax
    movq %rax, %rax
    movq %rbp, %rsp
    popq %rbp
    ret
    |}]
;;

let%expect_test "binary operations" =
  check
    {|
    int first() {
      int first = 12 + 1234 % 1234 * 12 / 2;
      int second = first + 12;
      second += first + second;
    }
  |};
  [%expect
    {|
    .text
    .globl _c0_main
    _c0_main:
    pushq %rbp
    movq %rsp, %rbp
    subq $0, %rsp
    movq $12, %rsi
    movq $1234, %rdi
    movq $1234, %rax
    movq %rax, %r11
    movq %rdi, %rax
    cqo
    idivq %r11
    movq %rdx, %rdi
    movq $12, %rax
    movq %rdi, %r11
    movq %rax, %rax
    imulq %r11
    movq %rax, %rdi
    movq $2, %rax
    movq %rax, %r11
    movq %rdi, %rax
    cqo
    idivq %r11
    movq %rax, %rax
    movq %rsi, %r11
    addq %rax, %r11
    movq %r11, %rdi
    movq %rdi, %rdi
    movq $12, %rax
    movq %rdi, %r11
    addq %rax, %r11
    movq %r11, %rax
    movq %rax, %rsi
    movq %rdi, %rdi
    movq %rax, %rax
    movq %rdi, %r11
    addq %rax, %r11
    movq %r11, %rax
    movq %rsi, %r11
    addq %rax, %r11
    movq %r11, %rax
    movq $0, %rax
    movq %rax, %rax
    movq %rbp, %rsp
    popq %rbp
    ret
    |}]
;;
