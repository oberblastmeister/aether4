open Std
module Tir = Ae_tir_types
module Entity = Ae_entity_std
module Id_gen = Entity.Id_gen
module Bag = Ae_data_bag
module Lir = Ae_lir_types
open Bag.Syntax
module Lower = Ae_monad_bag_writer.Make (Lir.Instr)
open Lower.Syntax

let empty = Bag.empty

type st =
  { gen : Lir.Temp_entity.Id.Witness.t Id_gen.t
  ; tir_to_lir : (Tir.Temp_entity.Id.Witness.t, Lir.Temp.t) Entity.Name.Table.t
  }

let create_state () = { gen = Id_gen.create (); tir_to_lir = Entity.Name.Table.create () }

let get_temp t (temp : Tir.Temp.t) : Lir.Temp.t =
  Entity.Name.Table.find_or_add t.tir_to_lir temp ~default:(fun () ->
    let id = Id_gen.next t.gen in
    let temp = Entity.Name.create temp.name id in
    temp)
;;

let fresh_temp ?(name = "fresh") t : Lir.Temp.t =
  let id = Id_gen.next t.gen in
  Entity.Name.create name id
;;

let lower_bin_op (op : Tir.Bin_op.t) : Lir.Bin_op.t =
  match op with
  | Add -> Add
  | Sub -> Sub
  | Mul -> Mul
  | Div -> Div
  | Mod -> Mod
;;

let rec lower_expr : _ -> Tir.Expr.t -> Lir.Expr.t Lower.t =
  fun st expr ->
  match expr with
  | IntConst i -> Lower.pure (Lir.Expr.IntConst i)
  | Bin { lhs; op; rhs } ->
    let+ lhs = lower_expr st lhs
    and+ rhs = lower_expr st rhs in
    let op = lower_bin_op op in
    Lir.Expr.Bin { lhs; op; rhs }
  | Temp temp ->
    let temp = get_temp st temp in
    Lower.pure (Lir.Expr.Temp temp)
;;

let lower_instr st (instr : Tir.Instr.t) : Lir.Instr.t Bag.t =
  match instr with
  | BlockParams { temps } ->
    empty +> [ Lir.Instr.BlockParams { temps = List.map temps ~f:(get_temp st) } ]
  | Assign { temp; e } ->
    let temp = get_temp st temp in
    let e_instr, e = lower_expr st e in
    e_instr +> [ Lir.Instr.Assign { temp; e } ]
  | Ret e ->
    let e_instr, e = lower_expr st e in
    e_instr +> [ Lir.Instr.Ret e ]
;;

let lower_block st (block : Tir.Block.t) : Lir.Block.t =
  let body = List.map block.body ~f:(lower_instr st) |> Bag.concat |> Bag.to_list in
  { body }
;;

let lower_func st (func : Tir.Func.t) : Lir.Func.t =
  let name = func.name in
  let blocks = func.blocks |> Entity.Name.Map.map ~f:(lower_block st) in
  let start = func.start in
  let next_id = Id_gen.next st.gen in
  { name; blocks; start; next_id = Entity.Id.unchecked_coerce next_id }
;;

let lower func =
  let st = create_state () in
  lower_func st func
;;
