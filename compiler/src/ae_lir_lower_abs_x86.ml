open Std
module Lir = Ae_lir_types
module Entity = Ae_entity_std
module Id_gen = Entity.Id_gen
module Abs_x86 = Ae_abs_x86_std
module Bag = Ae_data_bag
module Table = Entity.Ident.Table
module Ident = Entity.Ident

let empty = Bag.empty

open Bag.Syntax

type st =
  { gen : Abs_x86.Vreg_entity.Witness.t Id_gen.t
  ; lir_to_abs_x86 : Abs_x86.Vreg.t Lir.Temp.Table.t
  }

let create_state () =
  { gen = Id_gen.create (); lir_to_abs_x86 = Entity.Ident.Table.create () }
;;

let fresh_temp ?name st : Abs_x86.Vreg.t = Entity.Ident.fresh ?name st.gen

let get_vreg st temp =
  Table.find_or_add st.lir_to_abs_x86 temp ~default:(fun () ->
    fresh_temp ~name:temp.name st)
;;

let get_operand st temp = Abs_x86.Operand.Reg (get_vreg st temp)
let fresh_operand ?name st = Abs_x86.Operand.Reg (fresh_temp ?name st)

let lower_instr st (instr : Lir.Instr.t) : Abs_x86.Instr.t Bag.t =
  match instr with
  | BlockParams { temps } ->
    empty +> [ Abs_x86.Instr.BlockMov { temps = List.map temps ~f:(get_vreg st) } ]
  | IntConst { dst; const } when Option.is_some (Int32.of_int64 const) ->
    let dst = get_operand st dst in
    empty +> Abs_x86.Instr.[ Mov { dst; src = Imm (Int32.of_int64_exn const) } ]
  | IntConst { dst; const } ->
    let dst = get_operand st dst in
    empty +> Abs_x86.Instr.[ MovAbs { dst; src = const } ]
  | Copy { dst; src } ->
    let dst = get_operand st dst in
    let src = get_operand st src in
    empty +> Abs_x86.Instr.[ Mov { dst; src } ]
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
    in
    empty +> [ Abs_x86.Instr.Bin { dst; src1; op; src2 } ]
  | Lir.Instr.Ret { src } ->
    let src = get_operand st src in
    empty +> [ Abs_x86.Instr.Ret { src } ]
  | _ -> todol [%here]
;;

let lower_block st (block : Lir.Block.t) : Abs_x86.Block.t =
  let body = List.map block.body ~f:(lower_instr st) |> Bag.concat |> Bag.to_list in
  { body }
;;

let lower_func st (func : Lir.Func.t) : Abs_x86.Func.t =
  let name = func.name in
  let blocks = Ident.Map.map func.blocks ~f:(lower_block st) in
  let start = func.start in
  let next_id = Id_gen.next st.gen in
  { name; blocks; start; next_id }
;;

let lower func =
  let st = create_state () in
  lower_func st func
;;
