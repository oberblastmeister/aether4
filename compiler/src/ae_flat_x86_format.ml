open Std
open Ae_flat_x86_types
module Size = Ae_x86_size
module Condition_code = Ae_x86_condition_code

let string_of_mach_reg8l (m : Mach_reg.t) =
  match m with
  | RAX -> "%al"
  | RBX -> "%bl"
  | RCX -> "%cl"
  | RDX -> "%dl"
  | RSP -> "%spl"
  | RBP -> "%bpl"
  | RSI -> "%sil"
  | RDI -> "%dil"
  | R8 -> "%r8b"
  | R9 -> "%r9b"
  | R10 -> "%r10b"
  | R11 -> "%r11b"
  | R12 -> "%r12b"
  | R13 -> "%r13b"
  | R14 -> "%r14b"
  | R15 -> "%r15b"
;;

let string_of_mach_reg64 (m : Mach_reg.t) =
  match m with
  | RAX -> "%rax"
  | RCX -> "%rcx"
  | RDX -> "%rdx"
  | RBX -> "%rbx"
  | RSP -> "%rsp"
  | RBP -> "%rbp"
  | RSI -> "%rsi"
  | RDI -> "%rdi"
  | R8 -> "%r8"
  | R9 -> "%r9"
  | R10 -> "%r10"
  | R11 -> "%r11"
  | R12 -> "%r12"
  | R13 -> "%r13"
  | R14 -> "%r14"
  | R15 -> "%r15"
;;

let string_of_mach_reg : Size.t -> _ = function
  | Byte -> string_of_mach_reg8l
  | Qword -> string_of_mach_reg64
  | _ -> todol [%here]
;;

let format_operand (operand : Operand.t) size =
  match operand with
  | Imm i -> [%string "$%{i#Int32}"]
  | Reg r -> string_of_mach_reg size r
  | Mem _addr -> todol [%here]
;;

let suffix_of_size (s : Size.t) =
  match s with
  | Qword -> "q"
  | Dword -> "l"
  | Word -> "w"
  | Byte -> "b"
;;

let string_of_cond (cond : Condition_code.t) =
  match cond with
  | E -> "e"
  | NE -> "ne"
  | B -> "b"
  | BE -> "be"
  | A -> "a"
  | AE -> "ae"
  | G -> "g"
  | GE -> "ge"
  | L -> "l"
  | LE -> "le"
;;

let format_instr (instr : Instr.t) =
  let op = format_operand in
  let suff = suffix_of_size in
  match instr with
  | Add { dst; src; size } -> [%string "add%{suff size} %{op src size}, %{op dst size}"]
  | Sub { dst; src; size } -> [%string "sub%{suff size} %{op src size}, %{op dst size}"]
  | And { dst; src; size } -> [%string "and%{suff size} %{op src size}, %{op dst size}"]
  | Or { dst; src; size } -> [%string "or%{suff size} %{op src size}, %{op dst size}"]
  | Xor { dst; src; size } -> [%string "xor%{suff size} %{op src size}, %{op dst size}"]
  | Imul { src; size } -> [%string "imul%{suff size} %{op src size}"]
  | Idiv { src; size } -> [%string "idiv%{suff size} %{op src size}"]
  | Mov { dst; src; size } -> [%string "mov%{suff size} %{op src size}, %{op dst size}"]
  | Mov_abs { dst; src } -> [%string "movabsq %{src#Int64}, %{op dst Size.Qword}"]
  | Movzx { dst; dst_size; src; src_size } ->
    [%string
      "movzx%{suff src_size}%{suff dst_size} %{op src src_size}, %{op dst dst_size}"]
  | Push { src; size } -> [%string "push%{suff size} %{op src size}"]
  | Pop { dst; size } -> [%string "pop%{suff size} %{op dst size}"]
  | Lea _ -> todol [%here]
  | Ret -> [%string "ret"]
  | Jmp label -> [%string "jmp %{label}"]
  | J { cc; label } -> [%string "j%{string_of_cond cc} %{label}"]
  | Cmp { src1; src2; size } ->
    (* in att assembly the operands are switched for this *)
    [%string "cmp%{suff size} %{op src2 size}, %{op src1 size}"]
  | Test { src1; src2; size } ->
    [%string "test%{suff size} %{op src2 size}, %{op src1 size}"]
  | Set { cc; dst } -> [%string "set%{string_of_cond cc} %{op dst Byte}"]
  | Directive s -> s
  | Cqo -> "cqo"
  | Label s -> [%string "%{s}:"]
  | Comment s -> [%string "# %{s}"]
;;

let format prog =
  let buf = Buffer.create 1000 in
  List.iter prog ~f:(fun (instr : Instr.t) ->
    (match instr with
     | Directive _ | Label _ | Comment _ -> ()
     | _ -> Buffer.add_string buf "    ");
    format_instr instr |> Buffer.add_string buf;
    Buffer.add_char buf '\n';
    ());
  Buffer.contents buf
;;
