open Std
module Lir = Ae_lir_types
module Entity = Ae_entity_std
module Id_gen = Entity.Id_gen
module Abs_x86 = Ae_abs_x86_std
module Bag = Ae_data_bag
module Table = Entity.Ident.Table
module Ident = Entity.Ident
module Label_entity = Ae_label_entity

let empty = Bag.empty

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

let fresh_temp ?name st : Abs_x86.Vreg.t = Entity.Ident.fresh ?name st.temp_gen

let get_vreg st temp =
  Table.find_or_add st.lir_to_abs_x86 temp ~default:(fun () ->
    fresh_temp ~name:temp.name st)
;;

let get_operand st temp = Abs_x86.Operand.Reg (get_vreg st temp)
let fresh_operand ?name st = Abs_x86.Operand.Reg (fresh_temp ?name st)

let lower_ty (ty : Lir.Ty.t) : Abs_x86.Size.t =
  match ty with
  | I64 -> Qword
  | I1 -> Byte
;;

let lower_block_call st (b : Lir.Block_call.t) : Abs_x86.Block_call.t =
  { label = b.label; args = List.map b.args ~f:(get_vreg st) }
;;

let lower_instr st (instr : Lir.Instr'.t) : Abs_x86.Instr.t Bag.t =
  match instr.i with
  | Block_params { temps } ->
    empty
    +> [ Abs_x86.Instr.Block_params
           { temps = List.map temps ~f:(fun (vreg, ty) -> get_vreg st vreg, lower_ty ty) }
       ]
  | Nop -> empty +> [ Abs_x86.Instr.Nop ]
  | Jump b ->
    let b = lower_block_call st b in
    empty +> Abs_x86.Instr.[ Jump b ]
  | Cond_jump { cond; b1; b2 } ->
    let cond = get_operand st cond in
    let b1 = lower_block_call st b1 in
    let b2 = lower_block_call st b2 in
    empty +> Abs_x86.Instr.[ Cond_jump { cond = Op cond; b1; b2 } ]
  | Int_const { dst; const; ty = I1 } ->
    let dst = get_operand st dst in
    if Int64.(const <> 0L && const <> 1L)
    then raise_s [%message "const was not I1" (const : int64)];
    empty
    +> Abs_x86.Instr.[ Mov { dst; src = Imm (Int32.of_int64_exn const); size = Byte } ]
  | Int_const { dst; const; ty = I64 } when Option.is_some (Int32.of_int64 const) ->
    let dst = get_operand st dst in
    empty
    +> Abs_x86.Instr.[ Mov { dst; src = Imm (Int32.of_int64_exn const); size = Qword } ]
  | Int_const { dst; const; ty = I64 } ->
    let dst = get_operand st dst in
    empty +> Abs_x86.Instr.[ Mov_abs { dst; src = const } ]
  | Unary { dst; op; src } ->
    (match op with
     | Copy ty ->
       let dst = get_operand st dst in
       let src = get_operand st src in
       empty +> Abs_x86.Instr.[ Mov { dst; src; size = lower_ty ty } ])
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
    in
    empty +> [ Abs_x86.Instr.Bin { dst; src1; op; src2 } ]
  | Ret { src; ty } ->
    let src = get_operand st src in
    empty +> [ Abs_x86.Instr.Ret { src; size = lower_ty ty } ]
;;

let lower_block st (block : Lir.Block.t) : Abs_x86.Block.t =
  let body =
    block
    |> Lir.Block.instrs
    |> Arrayp.to_list
    |> List.map ~f:(fun i ->
      lower_instr st i |> Bag.map ~f:Abs_x86.Instr'.create_unindexed)
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
