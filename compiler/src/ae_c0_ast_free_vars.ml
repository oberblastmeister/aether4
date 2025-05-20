open Std
module Ast = Ae_c0_ast
open Ast

let rec expr_free_vars (expr : expr) =
  match expr with
  | Null _ | Int_const _ | Bool_const _ | Nullary _ -> Var.Set.empty
  | Ternary { cond; then_expr; else_expr; _ } ->
    expr_free_vars cond
    |> Set.union (expr_free_vars then_expr)
    |> Set.union (expr_free_vars else_expr)
  | Bin { lhs = expr1; rhs = expr2; _ } | Index { expr = expr1; index = expr2; _ } ->
    expr_free_vars expr1 |> Set.union (expr_free_vars expr2)
  | Call { args; _ } ->
    List.fold_left args ~init:Var.Set.empty ~f:(fun acc arg ->
      Set.union acc (expr_free_vars arg))
  | Var { var; _ } -> Var.Set.singleton var
  | Field_access { expr; _ } | Deref { expr; _ } | Alloc_array { expr; _ } ->
    expr_free_vars expr
;;

let rec stmt_free_vars (stmt : stmt) =
  match stmt with
  | If { cond; body1; body2; span = _ } ->
    expr_free_vars cond
    |> Set.union (stmt_free_vars body1)
    |> Set.union (Option.value_map ~default:Var.Set.empty ~f:stmt_free_vars body2)
  | Block block -> block_free_vars block
  | While { cond; body; span = _ } ->
    expr_free_vars cond |> Set.union (stmt_free_vars body)
  | Effect expr -> expr_free_vars expr
  | Return { expr; _ } -> Option.value_map ~default:Var.Set.empty ~f:expr_free_vars expr
  | Break _ | Declare _ -> Var.Set.empty
  | Assign { lvalue; expr; _ } -> expr_free_vars lvalue |> Set.union (expr_free_vars expr)
  | Assert { expr; _ } -> expr_free_vars expr
  | Par { block1; block2; _ } ->
    block_free_vars block1 |> Set.union (block_free_vars block2)

and block_free_vars (block : block) =
  List.fold_right block.stmts ~init:Var.Set.empty ~f:(fun stmt acc ->
    let acc = Set.union acc (stmt_free_vars stmt) in
    match stmt with
    | Declare { var; _ } -> Set.remove acc var
    | _ -> acc)
;;
