(*
   prefer 32 bit registers: https://stackoverflow.com/questions/41573502/why-doesnt-gcc-use-partial-registers
*)
open Std
module Lir = Ae_lir_types
module Entity = Ae_entity_std
module Id_gen = Entity.Id_gen
module Abs_x86 = Ae_abs_x86_std
module Bag = Ae_data_bag
module Table = Entity.Ident.Table
module Ident = Entity.Ident
module Label_entity = Ae_label_entity
open Ae_trace

let empty = Bag.empty
let ins = Abs_x86.Instr'.create_unindexed

open Bag.Syntax

type st =
  { temp_gen : Abs_x86.Vreg_entity.Witness.t Id_gen.t
  ; label_gen : Label_entity.Witness.t Id_gen.t
  ; lir_to_abs_x86 : Abs_x86.Vreg.t Lir.Temp.Table.t
  }

let create_state func =
  { temp_gen = Id_gen.create ()
  ; lir_to_abs_x86 = Entity.Ident.Table.create ()
  ; label_gen = Id_gen.of_id func.Lir.Func.next_label_id
  }
;;

let fresh_temp ?name ?info st : Abs_x86.Vreg.t =
  Entity.Ident.fresh ?name ?info st.temp_gen
;;

let get_vreg st temp =
  Table.find_or_add st.lir_to_abs_x86 temp ~default:(fun () ->
    fresh_temp ~name:temp.name ?info:temp.info st)
;;

let get_operand st temp = Abs_x86.Operand.Reg (get_vreg st temp)
let fresh_operand ?name ?info st = Abs_x86.Operand.Reg (fresh_temp ?name ?info st)

let lower_ty (ty : Lir.Ty.t) : Abs_x86.Size.t =
  match ty with
  | I64 -> Qword
  | I1 -> Dword
;;

let lower_block_call st (b : Lir.Block_call.t) : Abs_x86.Block_call.t =
  { label = b.label; args = List.map b.args ~f:(get_vreg st) }
;;

let lower_instr st (instr : Lir.Instr'.t) : Abs_x86.Instr'.t Bag.t =
  let ins = ins ?info:instr.info in
  match instr.i with
  | Unreachable -> empty +> [ ins Unreachable ]
  | Block_params { temps } ->
    empty
    +> [ ins
           (Block_params
              { temps =
                  List.map temps ~f:(fun (vreg, ty) -> get_vreg st vreg, lower_ty ty)
              })
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
  | Nullary { dst; op } ->
    (match op with
     | Int_const { const; ty = I1 } ->
       let dst = get_operand st dst in
       if Int64.(const <> 0L && const <> 1L)
       then raise_s [%message "const was not I1" (const : int64)];
       empty +> [ ins (Mov { dst; src = Imm (Int32.of_int64_exn const); size = Dword }) ]
     | Int_const { const; ty = I64 } when Option.is_some (Int32.of_int64 const) ->
       let dst = get_operand st dst in
       empty +> [ ins (Mov { dst; src = Imm (Int32.of_int64_exn const); size = Qword }) ]
     | Int_const { const; ty = I64 } ->
       let dst = get_operand st dst in
       empty +> [ ins (Mov_abs { dst; src = const }) ])
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
    empty +> [ ins (Abs_x86.Instr.Bin { dst; src1; op; src2 }) ]
  | Ret { src; ty } ->
    let src = get_operand st src in
    empty +> [ ins (Abs_x86.Instr.Ret { src; size = lower_ty ty }) ]
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

let lower_func st (func : Lir.Func.t) : Abs_x86.Func.t =
  let name = func.name in
  let blocks = Ident.Map.map func.blocks ~f:(lower_block st) in
  let start = func.start in
  let next_temp_id = Id_gen.next st.temp_gen in
  { name; blocks; start; next_temp_id; next_label_id = func.next_label_id }
;;

let lower func =
  let st = create_state func in
  lower_func st func
;;
