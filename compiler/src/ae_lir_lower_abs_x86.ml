open Std
module Lir = Ae_lir_types
module Abs_x86 = Ae_abs_x86_std
module Bag = Ae_data_bag
module Label = Ae_label
module Temp = Ae_temp
module X86_call_conv = Ae_x86_call_conv
module Call_conv = Ae_x86_call_conv
open Ae_trace

let empty = Bag.empty
let ins = Abs_x86.Instr'.create_unindexed

open Bag.Syntax

type st =
  { temp_gen : Temp.Id_gen.t
  ; label_gen : Label.Id_gen.t
  }

let create_state func =
  { temp_gen = Lir.Func.create_temp_gen func; label_gen = Lir.Func.create_label_gen func }
;;

let fresh_temp ?name ?info st : Abs_x86.Temp.t = Temp.fresh ?name ?info st.temp_gen
let get_operand _st temp = Abs_x86.Operand.Reg temp
let fresh_operand ?name ?info st = Abs_x86.Operand.Reg (fresh_temp ?name ?info st)

let lower_ty (ty : Lir.Ty.t) : Abs_x86.Ty.t =
  match ty with
  | I64 -> Qword
  | I1 -> Byte
;;

let lower_block_call _st (b : Lir.Block_call.t) : Abs_x86.Block_call.t =
  { label = b.label; args = List.map b.args ~f:(fun temp -> Abs_x86.Location.Temp temp) }
;;

let lower_instr st (instr : Lir.Instr'.t) : Abs_x86.Instr'.t Bag.t =
  let ins = ins ?info:instr.info in
  match instr.i with
  | Unreachable -> empty +> [ ins Unreachable ]
  | Call { dsts; func; args; call_conv } ->
    let dsts =
      List.map dsts ~f:(Tuple2.map_both ~f1:Abs_x86.Location.temp ~f2:lower_ty)
    in
    let args =
      List.map args ~f:(Tuple2.map_both ~f1:Abs_x86.Location.temp ~f2:lower_ty)
    in
    empty +> [ ins (Call { dsts; func; args; call_conv }) ]
  | Block_params params ->
    empty
    +> [ ins
           (Block_params
              (List.map params ~f:(fun param ->
                 let ty = lower_ty param.Lir.Block_param.ty in
                 { Abs_x86.Block_param.param = Temp param.param; ty })))
       ]
  | Nop -> empty +> [ ins Nop ]
  | Jump b ->
    let b = lower_block_call st b in
    empty +> [ ins (Jump b) ]
  | Cond_jump { cond; b1; b2 } ->
    let cond = get_operand st cond in
    let b1 = lower_block_call st b1 in
    let b2 = lower_block_call st b2 in
    empty +> [ ins (Cond_jump { cond = Op cond; b1; b2 }) ]
  | Nullary { dst; op } -> begin
    match op with
    | Int_const { const; ty = I1 } ->
      let dst = get_operand st dst in
      if Int64.(const <> 0L && const <> 1L)
      then raise_s [%message "const was not I1" (const : int64)];
      empty +> [ ins (Mov { dst; src = Imm (Int32.of_int64_exn const); size = Byte }) ]
    | Int_const { const; ty = I64 } when Option.is_some (Int32.of_int64 const) ->
      let dst = get_operand st dst in
      empty +> [ ins (Mov { dst; src = Imm (Int32.of_int64_exn const); size = Qword }) ]
    | Int_const { const; ty = I64 } ->
      let dst = get_operand st dst in
      empty +> [ ins (Mov_abs { dst; src = const }) ]
    | Undefined ty ->
      let dst = get_operand st dst in
      let ty = lower_ty ty in
      empty +> [ ins (Undefined { dst; size = ty }) ]
  end
  | Unary { dst; op; src } ->
    (match op with
     | Copy ty ->
       let dst = get_operand st dst in
       let src = get_operand st src in
       empty +> [ ins (Mov { dst; src; size = lower_ty ty }) ])
  | Bin { dst; src1; op; src2 } ->
    let dst = get_operand st dst in
    let src1 = get_operand st src1 in
    let src2 = get_operand st src2 in
    let op : Abs_x86.Bin_op.t =
      match op with
      | Add -> Add
      | Sub -> Sub
      | Mul -> Imul
      | Div -> Idiv
      | Mod -> Imod
      | Lt -> Lt
      | Gt -> Gt
      | Le -> Le
      | Ge -> Ge
      | And ty -> And (lower_ty ty)
      | Or ty -> Or (lower_ty ty)
      | Xor ty -> Xor (lower_ty ty)
      | Eq ty -> Eq (lower_ty ty)
      | Lshift -> Lshift
      | Rshift -> Rshift
    in
    empty +> [ ins (Bin { dst; src1; op; src2 }) ]
  | Ret { srcs; call_conv } ->
    let srcs =
      List.map srcs ~f:(fun (temp, ty) -> Abs_x86.Location.Temp temp, lower_ty ty)
    in
    empty +> [ ins (Ret { srcs; call_conv }) ]
;;

let lower_block st (block : Lir.Block.t) : Abs_x86.Block.t =
  let body =
    block
    |> Lir.Block.instrs
    |> Arrayp.to_list
    |> List.map ~f:(lower_instr st)
    |> Bag.concat
    |> Bag.to_arrayp
  in
  Abs_x86.Block.create block.label body
;;

let lower_func (func : Lir.Func.t) : Abs_x86.Func.t =
  let st = create_state func in
  let name = func.name in
  let blocks = Map.map func.blocks ~f:(lower_block st) in
  let start = func.start in
  let next_temp_id = Temp.Id_gen.get st.temp_gen in
  let call_conv = func.call_conv in
  { name
  ; blocks
  ; start
  ; next_temp_id
  ; next_label_id = func.next_label_id
  ; next_stack_slot_id = 0
  ; stack_slots = []
  ; call_conv
  }
;;

let lower_program (program : Lir.Program.t) : Abs_x86.Program.t =
  let funcs = List.map program.funcs ~f:lower_func in
  { funcs }
;;
