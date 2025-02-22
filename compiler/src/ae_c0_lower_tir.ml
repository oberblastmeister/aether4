open Std

(* module Cst = Ae_c0_cst *)
module Ast = Ae_c0_ast
module Tir = Ae_tir_types
module Entity = Ae_entity_std
module Temp = Tir.Temp
module Id_gen = Entity.Id_gen
module Bag = Ae_data_bag
open Bag.Syntax

let empty = Bag.empty

type st =
  { gen : Tir.Temp_entity.Witness.t Id_gen.t
  ; var_to_temp : Temp.t Ast.Var.Table.t
  ; label_gen : Tir.Label_entity.Witness.t Id_gen.t
  }

let create_state () =
  { gen = Id_gen.create ()
  ; var_to_temp = Ast.Var.Table.create ()
  ; label_gen = Id_gen.create ()
  }
;;

let var_temp t var =
  Hashtbl.find_or_add t.var_to_temp var ~default:(fun () ->
    let id = Id_gen.next t.gen in
    let temp = { Entity.Ident.id; name = var.name } in
    temp)
;;

let fresh_temp ?(name = "fresh") t : Temp.t =
  let id = Id_gen.next t.gen in
  Entity.Ident.create name id
;;

let lower_ty (ty : Ast.ty) : Tir.Ty.t =
  match ty with
  | Int -> Int
  | Bool -> Bool
;;

let rec lower_program st (program : Ast.program) : Tir.Func.t =
  let name = program.name in
  let start_label = Id_gen.next st.label_gen |> Entity.Ident.create "start" in
  let instrs =
    empty +> [ Tir.Instr.BlockParams { temps = [] } ] ++ lower_block st program.block
  in
  let start_block =
    instrs |> Bag.to_arrayp |> Arrayp.map ~f:Tir.Instr'.create |> Tir.Block.of_array
  in
  let func : Tir.Func.t =
    { name
    ; blocks = Entity.Ident.Map.singleton start_label start_block
    ; start = start_label
    ; next_id = Id_gen.next st.gen
    }
  in
  func

and lower_block st (block : Ast.block) =
  block |> List.map ~f:(lower_stmt st) |> Bag.concat

and lower_stmt st (stmt : Ast.stmt) =
  match stmt with
  | Declare { ty = _; var } ->
    let _ = var_temp st var in
    empty
  | Assign { lvalue; expr } ->
    let temp = var_temp st lvalue in
    lower_expr st temp expr
  | Block block -> lower_block st block
  | Return expr -> lower_return st expr
  | _ -> todol [%here]

and lower_return st expr =
  let dst = fresh_temp ~name:"ret" st in
  let ty = Ast.expr_ty_exn expr in
  let expr_i = lower_expr st dst expr in
  expr_i +> Tir.Instr.[ Ret { src = dst; ty = lower_ty ty } ]

and lower_bin_op (op : Ast.bin_op) : Tir.Bin_op.t =
  match op with
  | Add -> Add
  | Sub -> Sub
  | Mul -> Mul
  | Div -> Div
  | Mod -> Mod

and lower_expr st (dst : Temp.t) (expr : Ast.expr) : Tir.Instr.t Bag.t =
  match expr with
  | IntConst const -> empty +> Tir.Instr.[ IntConst { dst; const } ]
  | Bin { lhs; op; rhs; ty = _ } ->
    let src1 = fresh_temp ~name:"lhs" st in
    let src2 = fresh_temp ~name:"rhs" st in
    let lhs_i = lower_expr st src1 lhs in
    let rhs_i = lower_expr st src2 rhs in
    let op = lower_bin_op op in
    empty ++ lhs_i ++ rhs_i +> Tir.Instr.[ Bin { dst; src1; op; src2 } ]
  | Ast.Var { var; ty } ->
    let src = var_temp st var in
    empty +> Tir.Instr.[ Unary { dst; op = Copy (lower_ty (Option.value_exn ty)); src } ]
;;

let lower p =
  let st = create_state () in
  lower_program st p
;;
