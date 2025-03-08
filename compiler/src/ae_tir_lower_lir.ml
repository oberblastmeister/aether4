open Std
module Tir = Ae_tir_types
module Entity = Ae_entity_std
module Label_entity = Ae_label_entity
module Id_gen = Entity.Id_gen
module Bag = Ae_data_bag
module Lir = Ae_lir_types
open Bag.Syntax

let empty = Bag.empty
let ins = Lir.Instr'.create_unindexed

type st =
  { temp_gen : Lir.Temp_entity.Witness.t Id_gen.t
  ; label_gen : Label_entity.Witness.t Id_gen.t
  ; tir_to_lir : (Tir.Temp_entity.Witness.t, Lir.Temp.t) Entity.Ident.Table.t
  }

type instrs = Lir.Instr'.t Bag.t

let create_state func =
  { temp_gen = Id_gen.create ()
  ; label_gen = Id_gen.of_id func.Tir.Func.next_label_id
  ; tir_to_lir = Entity.Ident.Table.create ()
  }
;;

let get_temp t (temp : Tir.Temp.t) : Lir.Temp.t =
  Entity.Ident.Table.find_or_add t.tir_to_lir temp ~default:(fun () ->
    let id = Id_gen.next t.temp_gen in
    let temp = Entity.Ident.create temp.name id in
    temp)
;;

let fresh_temp ?(name = "fresh") t : Lir.Temp.t =
  let id = Id_gen.next t.temp_gen in
  Entity.Ident.create name id
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
  | And -> And
  | Or -> Or
  | Xor -> Xor
  | Eq -> Eq
  | Lshift -> Lshift
  | Rshift -> Rshift
;;

let lower_ty (ty : Tir.Ty.t) : Lir.Ty.t =
  match ty with
  | Int -> I64
  | Bool -> I1
;;

let lower_block_call st (b : Tir.Block_call.t) : Lir.Block_call.t =
  { label = b.label; args = List.map b.args ~f:(get_temp st) }
;;

let lower_instr st (instr : Tir.Instr'.t) : instrs =
  match instr.i with
  | Nop -> empty
  | Jump b ->
    let b = lower_block_call st b in
    empty +> [ ins (Jump b) ]
  | Cond_jump { cond; b1; b2 } ->
    let cond = get_temp st cond in
    let b1 = lower_block_call st b1 in
    let b2 = lower_block_call st b2 in
    empty +> [ ins (Cond_jump { cond; b1; b2 }) ]
  | Block_params { temps } ->
    empty
    +> [ ins
           (Lir.Instr.Block_params
              { temps =
                  List.map ~f:(fun (temp, ty) -> get_temp st temp, lower_ty ty) temps
              })
       ]
  | Nullary { dst; op } ->
    let dst = get_temp st dst in
    (match op with
     | Int_const const -> empty +> [ ins (Int_const { dst; const; ty = I64 }) ]
     | Bool_const const ->
       let const =
         match const with
         | true -> 1L
         | false -> 0L
       in
       empty +> [ ins (Int_const { dst; const; ty = I1 }) ])
  | Unary { dst; op; src } ->
    let dst = get_temp st dst in
    let src = get_temp st src in
    (match op with
     | Copy ty -> empty +> [ ins (Unary { dst; op = Copy (lower_ty ty); src }) ])
  | Bin { dst; op; src1; src2 } ->
    let dst = get_temp st dst in
    let src1 = get_temp st src1 in
    let src2 = get_temp st src2 in
    let op = lower_bin_op op in
    let instr = ins (Bin { dst; op; src1; src2 }) in
    empty +> [ instr ]
  | Ret { src; ty } ->
    let src = get_temp st src in
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

let lower_func st (func : Tir.Func.t) : Lir.Func.t =
  let name = func.name in
  let blocks = func.blocks |> Entity.Ident.Map.map ~f:(lower_block st) in
  let start = func.start in
  let next_temp_id = Id_gen.next st.temp_gen in
  let next_label_id = Id_gen.next st.label_gen in
  { name; blocks; start; next_temp_id; next_label_id }
;;

let lower func =
  let st = create_state func in
  lower_func st func
;;
