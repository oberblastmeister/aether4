open Std
module Tir = Ae_tir_types
module Entity = Ae_entity_std
module Id_gen = Entity.Id_gen
module Bag = Ae_data_bag
module Lir = Ae_lir_types
open Bag.Syntax

let empty = Bag.empty

type st =
  { gen : Lir.Temp_entity.Witness.t Id_gen.t
  ; tir_to_lir : (Tir.Temp_entity.Witness.t, Lir.Temp.t) Entity.Ident.Table.t
  }

let create_state () =
  { gen = Id_gen.create (); tir_to_lir = Entity.Ident.Table.create () }
;;

let get_temp t (temp : Tir.Temp.t) : Lir.Temp.t =
  Entity.Ident.Table.find_or_add t.tir_to_lir temp ~default:(fun () ->
    let id = Id_gen.next t.gen in
    let temp = Entity.Ident.create temp.name id in
    temp)
;;

let fresh_temp ?(name = "fresh") t : Lir.Temp.t =
  let id = Id_gen.next t.gen in
  Entity.Ident.create name id
;;

let lower_bin_op (op : Tir.Bin_op.t) : Lir.Bin_op.t =
  match op with
  | Add -> Add
  | Sub -> Sub
  | Mul -> Mul
  | Div -> Div
  | Mod -> Mod
;;

let lower_ty (ty : Tir.Ty.t) : Lir.Ty.t =
  match ty with
  | Int -> I64
  | Bool -> I1
;;

let lower_instr st (instr : Tir.Instr.t) : Lir.Instr.t Bag.t =
  match instr with
  | BlockParams { temps } ->
    empty
    +> [ Lir.Instr.BlockParams
           { temps = List.map ~f:(fun (temp, ty) -> get_temp st temp, lower_ty ty) temps }
       ]
  | IntConst { dst; const } ->
    let dst = get_temp st dst in
    empty +> Lir.Instr.[ IntConst { dst; const } ]
  | Unary { dst; op; src } ->
    let dst = get_temp st dst in
    let src = get_temp st src in
    (match op with
     | Copy ty -> empty +> Lir.Instr.[ Unary { dst; op = Copy (lower_ty ty); src } ])
  | Bin { dst; op; src1; src2 } ->
    let dst = get_temp st dst in
    let src1 = get_temp st src1 in
    let src2 = get_temp st src2 in
    let op = lower_bin_op op in
    let instr = Lir.Instr.Bin { dst; op; src1; src2 } in
    empty +> [ instr ]
  | Ret { src; ty } ->
    let src = get_temp st src in
    let ty = lower_ty ty in
    empty +> Lir.Instr.[ Ret { src; ty } ]
;;

let lower_block st (block : Tir.Block.t) : Lir.Block.t =
  let body = List.map block.body ~f:(lower_instr st) |> Bag.concat |> Bag.to_list in
  { body }
;;

let lower_func st (func : Tir.Func.t) : Lir.Func.t =
  let name = func.name in
  let blocks = func.blocks |> Entity.Ident.Map.map ~f:(lower_block st) in
  let start = func.start in
  let next_id = Id_gen.next st.gen in
  { name; blocks; start; next_id = Entity.Id.unchecked_coerce next_id }
;;

let lower func =
  let st = create_state () in
  lower_func st func
;;
