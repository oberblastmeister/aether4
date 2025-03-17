(* TODO: reformulate this as a forward analysis *)
open Std
module Ast = Ae_c0_ast

exception Exn of Sexp.t

let throw_s s = raise (Exn s)

let rec check_block (block : Ast.block) =
  let rec go stmts =
    match stmts with
    | stmt :: stmts ->
      let live = go stmts in
      check_stmt live stmt
    | [] -> Ast.Var.Set.empty
  in
  go block

and iter_expr_uses (expr : Ast.expr) ~f =
  match expr with
  | Ternary { cond; then_expr; else_expr; ty = _; span = _ } ->
    iter_expr_uses cond ~f;
    iter_expr_uses then_expr ~f;
    iter_expr_uses else_expr ~f;
    ()
  | Var { var; ty = _ } ->
    f var;
    ()
  | Int_const _ | Bool_const _ -> ()
  | Bin { lhs; op = _; rhs; ty = _; span = _ } ->
    iter_expr_uses lhs ~f;
    iter_expr_uses rhs ~f;
    ()

and expr_uses_set expr = iter_expr_uses expr |> Iter.to_list |> Ast.Var.Set.of_list

and check_stmt live (stmt : Ast.stmt) =
  match stmt with
  | If { cond; body1; body2; span = _ } ->
    let live1 = check_stmt live body1 in
    let live2 = Option.value_map body2 ~f:(check_stmt live) ~default:Ast.Var.Set.empty in
    let cond_uses = expr_uses_set cond in
    live1 |> Set.union live2 |> Set.union cond_uses
  | Block { block; span = _ } ->
    List.fold_right ~init:live ~f:(fun stmt live -> check_stmt live stmt) block
  | Ast.While { cond; body; span = _ } ->
    let live_body = check_stmt live body in
    let cond_uses = expr_uses_set cond in
    live |> Set.union live_body |> Set.union cond_uses
  | Effect expr -> Set.union live (expr_uses_set expr)
  | Return { expr; span = _ } ->
    (* just discard the live set, because nothing is live before it *)
    expr_uses_set expr
  | Declare { ty = _; var; span = _ } ->
    if Set.mem live var
    then throw_s [%message "Variable was used before initialized" (var : Ast.var)]
    else live
  | Assign { lvalue; expr; span = _ } ->
    live |> Fn.flip Set.remove lvalue |> Set.union (expr_uses_set expr)
;;

let check_program (program : Ast.program) =
  match check_block program.block |> ignore with
  | exception Exn s -> error_s s
  | _ -> Ok ()
;;
