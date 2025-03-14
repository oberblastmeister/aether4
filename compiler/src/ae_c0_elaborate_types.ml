open Std
module Ast = Ae_c0_ast

exception Exn of Sexp.t

type st = { context : Ast.ty Ast.Var.Map.t }

let create_state () = { context = Ast.Var.Map.empty }
let throw_s s = raise (Exn s)

let var_ty st var =
  Map.find st.context var
  |> Option.value_or_thunk ~default:(fun () ->
    throw_s [%message "Var not found!" (var : Ast.Var.t)])
;;

let check_ty_eq _st ty ty' =
  if Ast.equal_ty ty ty'
  then ()
  else throw_s [%message "Types were not equal" (ty : Ast.ty) (ty' : Ast.ty)]
;;

let rec infer_expr st (expr : Ast.expr) : Ast.expr =
  match expr with
  | Var { var; ty = _ } -> Var { var; ty = Some (var_ty st var) }
  | Int_const _ | Bool_const _ -> expr
  | Ternary { cond; then_expr; else_expr; ty = _ } ->
    let cond = check_expr st cond Bool in
    let then_expr = infer_expr st then_expr in
    let else_expr = infer_expr st else_expr in
    check_ty_eq st (Ast.expr_ty_exn then_expr) (Ast.expr_ty_exn else_expr);
    Ternary { cond; then_expr; else_expr; ty = Some (Ast.expr_ty_exn then_expr) }
  | Bin { lhs; op; rhs; ty = _ } ->
    (match op with
     | Add | Sub | Mul | Div | Mod | Bit_and | Bit_or | Bit_xor | Lshift | Rshift ->
       let lhs = check_expr st lhs Int in
       let rhs = check_expr st rhs Int in
       Bin { lhs; op; rhs; ty = Some Int }
     | Lt | Gt | Le | Ge ->
       let lhs = check_expr st lhs Int in
       let rhs = check_expr st rhs Int in
       Bin { lhs; op; rhs; ty = Some Bool }
     | Eq ->
       let lhs = infer_expr st lhs in
       let rhs = infer_expr st rhs in
       check_ty_eq st (Ast.expr_ty_exn lhs) (Ast.expr_ty_exn rhs);
       Bin { lhs; op; rhs; ty = Some Bool })

and check_expr st (expr : Ast.expr) (ty : Ast.ty) : Ast.expr =
  let expr = infer_expr st expr in
  check_ty_eq st ty (Ast.expr_ty_exn expr);
  expr
;;

let rec check_stmt st (stmt : Ast.stmt) : Ast.stmt =
  match stmt with
  | If { cond; body1; body2 } ->
    let cond = check_expr st cond Bool in
    let body1 = check_stmt st body1 in
    let body2 = Option.map body2 ~f:(check_stmt st) in
    If { cond; body1; body2 }
  | Block block -> Block (check_block st block)
  | While { cond; body } ->
    let cond = check_expr st cond Bool in
    let body = check_stmt st body in
    While { cond; body }
  | Return expr -> Return (check_expr st expr Int)
  | Effect e ->
    let e = infer_expr st e in
    Effect e
  | Declare _ -> stmt
  | Assign { lvalue; expr } ->
    let ty = infer_expr st (Var { var = lvalue; ty = None }) |> Ast.expr_ty_exn in
    let expr = check_expr st expr ty in
    Assign { lvalue; expr }

and check_block st (block : Ast.block) : Ast.block =
  let rec loop st stmts =
    match stmts with
    | [] -> []
    | stmt :: stmts ->
      let stmt = check_stmt st stmt in
      let st' =
        match stmt with
        | Declare { ty; var } ->
          { st with context = Map.add_exn st.context ~key:var ~data:ty }
        | _ -> st
      in
      let stmts = loop st' stmts in
      stmt :: stmts
  in
  loop st block
;;

let check_program st (prog : Ast.program) =
  check_ty_eq st Int prog.ty;
  let block = check_block st prog.block in
  { prog with block }
;;

let check_program prog =
  let st = create_state () in
  match check_program st prog with
  | prog -> Ok prog
  | exception Exn s -> error_s s
;;
