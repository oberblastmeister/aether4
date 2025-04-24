open Std
module Tir = Ae_tir_types
module Label = Ae_label
module Temp = Ae_temp
module Bag = Ae_data_bag
module Lir = Ae_lir_types
module X86_call_conv = Ae_x86_call_conv
open Bag.Syntax

let empty = Bag.empty
let ins = Lir.Instr'.create_unindexed

type st =
  { temp_gen : Temp.Id_gen.t
  ; label_gen : Label.Id_gen.t
  }

type instrs = Lir.Instr'.t Bag.t

let create_state func =
  { temp_gen = Tir.Func.create_temp_gen func
  ; label_gen = Label.Id_gen.create func.Tir.Func.next_label_id
  }
;;

let fresh_temp ?(name = "fresh") ?info t : Lir.Temp.t =
  let id = Temp.Id_gen.get t.temp_gen in
  Temp.create ?info name id
;;

let lower_ty (ty : Tir.Ty.t) : Lir.Ty.t =
  match ty with
  | Int -> I64
  | Bool -> I1
;;

let lower_bin_op (op : Tir.Bin_op.t) : Lir.Bin_op.t =
  match op with
  | Add -> Add
  | Sub -> Sub
  | Mul -> Mul
  | Div -> Div
  | Mod -> Mod
  | Lt -> Lt
  | Gt -> Gt
  | Le -> Le
  | Ge -> Ge
  | And -> And I64
  | Or -> Or I64
  | Xor -> Xor I64
  | Eq ty -> Eq (lower_ty ty)
  | Lshift -> Lshift
  | Rshift -> Rshift
;;

let lower_block_call (b : Tir.Block_call.t) : Lir.Block_call.t =
  { label = b.label; args = b.args }
;;

let mangle_func_name name = "_c0_" ^ name

let lower_instr _st (instr : Tir.Instr'.t) : instrs =
  let ins = ins ?info:instr.info in
  match instr.i with
  | Nop -> empty
  | Call { dst; ty; func; args; is_extern } ->
    let func = mangle_func_name func in
    let ty = lower_ty ty in
    let args = (List.map & Tuple2.map_snd) args ~f:lower_ty in
    let call_conv = if is_extern then X86_call_conv.sysv else X86_call_conv.c0 in
    empty +> [ ins (Call { dst; ty; func; args; call_conv }) ]
  | Unreachable -> empty +> [ ins Unreachable ]
  | Jump b ->
    let b = lower_block_call b in
    empty +> [ ins (Jump b) ]
  | Cond_jump { cond; b1; b2 } ->
    let b1 = lower_block_call b1 in
    let b2 = lower_block_call b2 in
    empty +> [ ins (Cond_jump { cond; b1; b2 }) ]
  | Block_params temps ->
    empty
    +> [ ins
           (Lir.Instr.Block_params
              (List.map
                 ~f:(fun { param; ty } ->
                   let ty = lower_ty ty in
                   { Lir.Block_param.param; ty })
                 temps))
       ]
  | Nullary { dst; op } ->
    (match op with
     | Int_const const ->
       empty +> [ ins (Nullary { dst; op = Int_const { const; ty = I64 } }) ]
     | Bool_const const ->
       let const =
         match const with
         | true -> 1L
         | false -> 0L
       in
       empty +> [ ins (Nullary { dst; op = Int_const { const; ty = I1 } }) ])
  | Unary { dst; op; src } ->
    (match op with
     | Copy ty -> empty +> [ ins (Unary { dst; op = Copy (lower_ty ty); src }) ])
  | Bin { dst; op; src1; src2 } ->
    let op = lower_bin_op op in
    let instr = ins (Bin { dst; op; src1; src2 }) in
    empty +> [ instr ]
  | Ret { src; ty } ->
    let ty = lower_ty ty in
    empty +> [ ins (Ret { src; ty }) ]
;;

let lower_block st (block : Tir.Block.t) : Lir.Block.t =
  let body =
    Arrayp.to_list (Tir.Block.instrs block)
    |> List.map ~f:(lower_instr st)
    |> Bag.concat
    |> Bag.to_arrayp
  in
  Lir.Block.create block.label body
;;

let lower_func (func : Tir.Func.t) : Lir.Func.t =
  let st = create_state func in
  let name = mangle_func_name func.name in
  let blocks = func.blocks |> Map.map ~f:(lower_block st) in
  let start = func.start in
  let next_temp_id = Temp.Id_gen.get st.temp_gen in
  let next_label_id = Label.Id_gen.get st.label_gen in
  let call_conv = X86_call_conv.c0 in
  { name; blocks; start; next_temp_id; next_label_id; call_conv }
;;

let lower_program (program : Tir.Program.t) : Lir.Program.t =
  let funcs = List.map program.funcs ~f:lower_func in
  { funcs }
;;
