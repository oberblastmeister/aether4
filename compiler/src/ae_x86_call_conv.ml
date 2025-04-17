open Ae_x86_mach_reg

let caller_saved_without_r11 = [ RAX; RDI; RSI; RDX; RCX; R8; R9; R10 ]
let caller_saved = caller_saved_without_r11 @ [ R11 ]
let callee_saved_without_stack = [ RBX; R12; R13; R14; R15 ]
let callee_saved = [ RSP; RBP ] @ callee_saved_without_stack
let args = [ RDI; RSI; RDX; RCX; R8; R9 ]
let ret = RAX
let regalloc_usable_mach_regs = caller_saved_without_r11
