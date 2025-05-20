(* TODO: reformulate this as a forward analysis *)
open Std
open Ae_trace
module Ast = Ae_c0_ast
open Ast

exception Exn of Sexp.t

let throw_s s = raise (Exn s)

type st =
  { declared : Ast.Var.Set.t
  ; declared_stack : Ast.Var.Set.t Var.Map.t
  }

let rec iter_expr_uses (expr : Ast.expr) ~f =
  match expr with
  | Null _ -> ()
  | Alloc_array { arg_ty = _; expr; ty = _; span = _ } -> iter_expr_uses expr ~f
  | Index { expr; index; span = _; ty = _ } ->
    iter_expr_uses expr ~f;
    iter_expr_uses index ~f
  | Ternary { cond; then_expr; else_expr; ty = _; span = _ } ->
    iter_expr_uses cond ~f;
    iter_expr_uses then_expr ~f;
    iter_expr_uses else_expr ~f;
    ()
  | Var { var; _ } -> f var
  | Deref { expr; ty = _; span = _ } ->
    iter_expr_uses expr ~f;
    ()
  | Field_access { expr; field = _; ty = _; span = _ } ->
    iter_expr_uses expr ~f;
    ()
  | Nullary _ | Int_const _ | Bool_const _ -> ()
  | Bin { lhs; op = _; rhs; ty = _; span = _ } ->
    iter_expr_uses lhs ~f;
    iter_expr_uses rhs ~f;
    ()
  | Call { func = _; args; ty = _; span = _ } ->
    (List.iter @> iter_expr_uses) args ~f;
    ()
;;

let expr_uses_set expr = iter_expr_uses expr |> Iter.to_list |> Ast.Var.Set.of_list

let check_expr expr defined =
  begin
    let@: use = iter_expr_uses expr in
    if not (Set.mem defined use)
    then throw_s [%message "Variable was used before initialized" ~var:(use : Ast.var)]
  end
;;

let rec check_block declared_stack defined (block : Ast.block) =
  let declared_stack, defined =
    List.fold
      block.stmts
      ~init:((block.label, Ast.Var.Set.empty) :: declared_stack, defined)
      ~f:(fun (declared_stack, defined) stmt ->
        let defined = check_stmt declared_stack defined stmt in
        match stmt with
        | Declare { var; _ } ->
          let (label, declared_last), declared_stack = List.uncons_exn declared_stack in
          let declared_stack = (label, Set.add declared_last var) :: declared_stack in
          declared_stack, defined
        | _ -> declared_stack, defined)
  in
  let (_label, declared), _declared_stack = List.uncons_exn declared_stack in
  Set.diff defined declared

and check_stmt
      (declared_stack : (Ast.Var.t option * Ast.Var.Set.t) list)
      defined
      (stmt : Ast.stmt)
  =
  match stmt with
  | Ast.If { cond; body1; body2; span = _ } ->
    check_expr cond defined;
    let defined1 = check_stmt declared_stack defined body1 in
    Option.value_map
      body2
      ~f:(fun stmt -> check_stmt declared_stack defined stmt |> Set.inter defined1)
      ~default:defined1
  | Ast.Block block -> check_block declared_stack defined block
  | Ast.While { cond; body; span = _ } ->
    check_expr cond defined;
    let _defined = check_stmt declared_stack defined body in
    defined
  | Ast.Effect expr ->
    check_expr expr defined;
    defined
  | Ast.Return { expr; span = _ } ->
    Option.iter expr ~f:(fun expr -> check_expr expr defined);
    let declared =
      List.map declared_stack ~f:(fun (_label, s) -> s) |> Var.Set.union_list
    in
    (* declares literally everything in scope *)
    declared
  | Ast.Declare _ -> defined
  | Ast.Assign { lvalue; expr; span = _ } ->
    check_expr expr defined;
    begin
      match lvalue with
      | Var { var; _ } -> Set.add defined var
      | _ -> defined
    end
  | Ast.Assert { expr; span = _ } ->
    check_expr expr defined;
    defined
  | Ast.Par { block1; block2; span = _ } ->
    (* par does not define anything, because we may run this stuff on different threads *)
    let _ = check_block declared_stack defined block1 in
    let _ = check_block declared_stack defined block2 in
    defined
  | Ast.Break { label; span = _ } ->
    let%fail_exn declared, (label', declared_at_label) :: _ =
      List.split_while declared_stack ~f:(fun (label', _) ->
        not ([%equal: Var.t option] label' (Some label)))
    in
    assert ([%equal: Var.t option] label' (Some label));
    let declared = declared_at_label :: List.map declared ~f:snd |> Var.Set.union_list in
    (*
       The declares everything up until the label.
      We have to union it with the defined because some things weren't defined
      in the scope of this label, or declared in this label.
    *)
    Set.union defined declared
;;

let check_global_decl (decl : Ast.global_decl) =
  match decl with
  | Func_defn func ->
    let params = List.map func.ty.params ~f:(fun param -> param.var) |> Var.Set.of_list in
    check_block [ None, params ] params func.body |> ignore
  | _ -> ()
;;

let check_program program = List.iter program ~f:check_global_decl

let check_program (program : Ast.program) =
  match check_program program with
  | exception Exn s -> error_s s
  | _ -> Ok ()
;;
