open Std
open Ae_x86_mach_reg

open struct
  module Call_conv = Ae_call_conv
end

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
  let return_reg = RAX in
  let call_clobbers =
    List.filter regalloc_usable_mach_regs ~f:(fun r -> not (equal return_reg r))
  in
  let num_call_clobbers = List.length call_clobbers in
  (* 13 call arguments *)
  let call_args = [ RDI; RSI; RCX; RDX; R8; R9; R10; R12; R13; R14; R15; RBX; RAX ] in
  let num_args_in_regs = List.length call_args in
  { Call_conv.return_reg; call_args; num_args_in_regs; call_clobbers; num_call_clobbers }
;;

let sysv =
  let call_args = [ RDI; RSI; RDX; RCX; R8; R9 ] in
  let return_reg = RAX in
  let call_clobbers =
    List.filter caller_saved ~f:(fun r ->
      (not (equal return_reg r)) && List.mem ~equal regalloc_usable_mach_regs r)
  in
  let num_call_clobbers = List.length call_clobbers in
  let num_args_in_regs = List.length call_args in
  { Call_conv.return_reg; call_args; num_args_in_regs; call_clobbers; num_call_clobbers }
;;
