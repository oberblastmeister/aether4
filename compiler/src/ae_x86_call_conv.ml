open Std
open Ae_x86_mach_reg

open struct
  module Call_conv = Ae_call_conv
end

(* invariant: every register that is in a constrained instruction must be in here *)
let regalloc_usable_mach_regs =
  [ RAX; RCX; RDX; RBX; RSI; RDI; R8; R9; R10; R12; R13; R14; R15 ]
;;

let regalloc_usable_colors =
  List.map regalloc_usable_mach_regs ~f:to_enum |> Int.Set.of_list
;;

let num_regs = List.length regalloc_usable_mach_regs

(* not including R11 *)
let caller_saved = [ RAX; RDI; RSI; RDX; RCX; R8; R9; R10 ]

(* not including RBP and RSP *)
let callee_saved = [ RBX; R12; R13; R14; R15 ]

let c0 =
  (* TODO: eventually set these to all of the available registers *)
  let return_regs = [ RAX; RBX ] in
  let call_clobbers = regalloc_usable_mach_regs in
  (* 13 call arguments *)
  let call_args = [ RAX; RBX; RDI; RSI; RCX; RDX; R8; R9; R10; R12; R13; R14; R15 ] in
  let num_args_in_regs = List.length call_args in
  { Call_conv.name = "c0"; return_regs; call_args; num_args_in_regs; call_clobbers }
;;

let sysv =
  let call_args = [ RDI; RSI; RDX; RCX; R8; R9 ] in
  let return_regs = [ RAX ] in
  let call_clobbers = caller_saved in
  let num_args_in_regs = List.length call_args in
  { Call_conv.name = "sysv"; return_regs; call_args; num_args_in_regs; call_clobbers }
;;
