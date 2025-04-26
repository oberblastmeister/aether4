open Std
open Ae_abs_x86_types
open Sexp_lang.Pretty

let stack_address (t : Stack_address.t) =
  match t with
  | Slot s -> list [ atom "slot"; atom (Stack_slot.to_string s) ]
  | Previous_frame i -> list [ atom "previous_frame"; atom (Int32.to_string_hum i) ]
  | Current_frame i -> list [ atom "current_frame"; atom (Int32.to_string_hum i) ]
;;

let operand (t : Operand.t) =
  match t with
  | Imm i -> atom (Int32.to_string_hum i)
  | Reg temp -> atom (Temp.to_string temp)
  | Mem _addr -> todol [%here]
  | Stack stack -> stack_address stack
;;

let temp t = atom (Temp.to_string t)

let size_to_string (size : Ty.t) =
  match size with
  | Qword -> "q"
  | Dword -> "d"
  | Word -> "w"
  | Byte -> "b"
;;

let int i = atom (Int.to_string_hum i)
let label l = atom (Label.to_string l)

let location (loc : Location.t) =
  match loc with
  | Temp t -> temp t
  | Stack s -> stack_address s
;;

let block_call (t : Block_call.t) = list ([ label t.label ] @ List.map ~f:location t.args)
let instr s size = atom (s ^ "." ^ size_to_string size)

let bin ?dst src1 (op : Bin_op.t) src2 =
  let op =
    match op with
    | Add -> instr "add" Qword
    | Sub -> instr "sub" Qword
    | Imul -> instr "imul" Qword
    | Idiv -> instr "idiv" Qword
    | Imod -> instr "imod" Qword
    | Lt -> instr "lt" Qword
    | Gt -> instr "gt" Qword
    | Le -> instr "le" Qword
    | Ge -> instr "ge" Qword
    | And ty -> instr "and" ty
    | Or ty -> instr "or" ty
    | Xor ty -> instr "xor" ty
    | Eq ty -> instr "eq" ty
    | Lshift -> instr "lshift" Qword
    | Rshift -> instr "rshift" Qword
  in
  list
    ([ op ] @ Option.to_list (Option.map dst ~f:operand) @ [ operand src1; operand src2 ])
;;

let cond_expr (t : Cond_expr.t) =
  match t with
  | Op o -> operand o
  | Bin { src1; op; src2 } -> bin src1 op src2
;;

let block_param (t : Block_param.t) =
  list [ location t.param; atom (size_to_string t.ty) ]
;;

let instr (t : Instr.t) =
  match t with
  | Block_params params -> list ([ atom "block_params" ] @ List.map params ~f:block_param)
  | Mov { dst; src; size } -> list [ instr "mov" size; operand dst; operand src ]
  | Nop -> list [ atom "nop" ]
  | Undefined { dst; size } -> list [ instr "undefined" size; operand dst ]
  | Push { src; size } -> list [ instr "push" size; temp src ]
  | Pop { dst; size } -> list [ instr "pop" size; temp dst ]
  | Mov_abs { dst; src } ->
    list [ atom "movabs.q"; operand dst; atom (Int64.to_string_hum src) ]
  | Bin { dst; op; src1; src2 } -> bin ~dst src1 op src2
  | Jump b -> list [ atom "jump"; block_call b ]
  | Cond_jump { cond; b1; b2 } ->
    list [ atom "cond_jump"; cond_expr cond; block_call b1; block_call b2 ]
  | Ret { src; size } -> list [ instr "ret" size; operand src ]
  | Unreachable -> list [ atom "unreachable" ]
  | Call { dst; size; func; args; call_conv } ->
    list
    @@ [ instr "call" size ]
    @ (if String.equal call_conv.name "c0"
       then []
       else [ Keyword "call_conv"; atom call_conv.name ])
    @ []
    @ [ temp dst
      ; list ([ atom func ] @ List.map ~f:(fun (loc, _ty) -> location loc) args)
      ]
;;

let block (t : Block.t) =
  let instrs = Arrayp.to_list t.body in
  let block_params, rest =
    ( List.hd instrs
      |> Option.value_exn ~message:"Expected block params as first instruction"
      |> Instr'.instr
      |> Instr.block_params_val
      |> Option.value_exn ~message:"Expected block params as first instruction"
    , List.drop instrs 1 )
  in
  let instrs =
    List.map rest ~f:(fun i -> instr i.i) |> List.intersperse ~sep:(Ann Line)
  in
  list
    ([ atom "block"
     ; list ([ label t.label ] @ List.map block_params ~f:block_param)
     ; Ann IndentLine
     ]
     @ instrs)
;;

let func (t : Func.t) =
  let blocks =
    Map.data t.blocks
    |> List.map ~f:(fun b -> block b)
    |> List.intersperse ~sep:(Ann Line)
  in
  list ([ atom "func"; atom t.name; Ann IndentLine ] @ blocks)
;;

let program (t : Program.t) =
  let funcs = List.map t.funcs ~f:func in
  list funcs
;;
