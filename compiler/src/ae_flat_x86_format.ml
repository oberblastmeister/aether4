open Std
open Ae_flat_x86_types
module Size = Ae_x86_size

let string_of_mach_reg8l = function
  | Mach_reg.RAX -> "al"
  | RBX -> "bl"
  | RCX -> "cl"
  | RDX -> "dl"
  | RSP -> "spl"
  | RBP -> "bpl"
  | RSI -> "sil"
  | RDI -> "dil"
  | R8 -> "r8b"
  | R9 -> "r9b"
  | R10 -> "r10b"
  | R11 -> "r11b"
  | R12 -> "r12b"
  | R13 -> "r13b"
  | R14 -> "r14b"
  | R15 -> "r15b"
;;

let string_of_mach_reg64 = function
  | Mach_reg.RAX -> "rax"
  | RCX -> "rcx"
  | RDX -> "rdx"
  | RBX -> "rbx"
  | RSP -> "rsp"
  | RBP -> "rbp"
  | RSI -> "rsi"
  | RDI -> "rdi"
  | R8 -> "r8"
  | R9 -> "r9"
  | R10 -> "r10"
  | R11 -> "r11"
  | R12 -> "r12"
  | R13 -> "r13"
  | R14 -> "r14"
  | R15 -> "r15"
;;

let string_of_mach_reg = function
  | Size.Byte -> string_of_mach_reg8l
  | Size.Qword -> string_of_mach_reg64
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

let format_instr (instr : Instr.t) =
  let op = format_operand in
  let suff = suffix_of_size in
  match instr with
  | Add { dst; src } ->
    let size = Size.Qword in
    [%string "add%{suff size} %{op src size}, %{op dst size}"]
  | Sub { dst; src } ->
    let size = Size.Qword in
    [%string "sub%{suff size} %{op src size}, %{op dst size}"]
  | Imul { src } ->
    let size = Size.Qword in
    [%string "imul%{suff size} %{op src size}"]
  | Idiv { src } ->
    let size = Size.Qword in
    [%string "idiv%{suff size} %{op src size}"]
  | Mov { dst; src } ->
    let size = Size.Qword in
    [%string "mov%{suff size} %{op src size}, %{op dst size}"]
  | MovAbs { dst; src } ->
    let size = Size.Qword in
    [%string "movabsq %{src#Int64}, %{op dst size}"]
  | Push { src } ->
    let size = Size.Qword in
    [%string "push%{suff size} %{op src size}"]
  | Pop { dst } ->
    let size = Size.Qword in
    [%string "pop%{suff size} %{op dst size}"]
  | Ret -> [%string "ret"]
  | Directive s -> s
  | Cqo -> "cqo"
  | Label s -> [%string "%{s}:"]
  | Comment s -> [%string "# %{s}"]
  | Lea _ -> todol [%here]
;;

let format prog =
  let buf = Buffer.create 1000 in
  List.iter prog ~f:(fun instr ->
    format_instr instr |> Buffer.add_string buf;
    Buffer.add_char buf '\n';
    ());
  Buffer.contents buf
;;
