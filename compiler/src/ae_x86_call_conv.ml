open Std
open Ae_x86_mach_reg

let caller_saved_without_r11 = [ RAX; RDI; RSI; RDX; RCX; R8; R9; R10 ]
let caller_saved = caller_saved_without_r11 @ [ R11 ]
let callee_saved_without_stack = [ RBX; R12; R13; R14; R15 ]
let callee_saved = [ RSP; RBP ] @ callee_saved_without_stack
let args = [ RDI; RSI; RDX; RCX; R8; R9 ]
let ret = RAX

let regalloc_usable_mach_regs =
  [ RAX; RCX; RDX; RBX; RSI; RDI; R8; R9; R10; R12; R13; R14; R15 ]
;;

let regalloc_usable_colors =
  List.map regalloc_usable_mach_regs ~f:to_enum |> Int.Set.of_list
;;

let num_regs = List.length regalloc_usable_mach_regs

(* Make sure R11 is not present in any of these lists below *)
(* our current calling convention *)
let return_register = RAX

let call_clobbers =
  List.filter regalloc_usable_mach_regs ~f:(fun r -> not (equal return_register r))
;;

let num_call_clobbers = List.length call_clobbers

(* this is in order of argument number *)
let call_arguments = [ RDI; RSI; RCX; RDX; R8; R9; R10; R12; R13; R14; R15; RBX; RAX ]
let num_arguments_in_registers = List.length call_arguments
