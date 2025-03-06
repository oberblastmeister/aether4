open Std
module Cst = Ae_c0_cst
module Ast = Ae_c0_ast
module Bag = Ae_data_bag
open Bag.Syntax

let empty = Bag.empty

exception Exn of Sexp.t

type st =
  { next_temp_id : int ref
  ; context : Ast.var String.Map.t
  }

let create_state () =
  let next_temp_id = ref 0 in
  let context = String.Map.empty in
  { next_temp_id; context }
;;

let elab_ty _st (ty : Cst.ty) : Ast.ty =
  match ty with
  | Bool -> Bool
  | Int -> Int
;;

let throw_s s = raise (Exn s)

let elab_var st (var : Cst.var) : Ast.var =
  Map.find st.context var
  |> Option.value_or_thunk ~default:(fun () ->
    throw_s [%message "Variable not found" (var : Cst.var)])
;;

let fresh_var st (var : Cst.var) : Ast.var =
  let id : int = !(st.next_temp_id) in
  st.next_temp_id := id + 1;
  { name = var; id }
;;

let declare_var st (var : Cst.var) : Ast.var * st =
  match Map.find st.context var with
  | None ->
    let var' = fresh_var st var in
    var', { st with context = Map.add_exn st.context ~key:var ~data:var' }
  | Some _ -> throw_s [%message "Var was already declared!" (var : Cst.var)]
;;

let rec elab_stmt st (stmt : Cst.stmt) : Ast.stmt Bag.t * st =
  match stmt with
  | Decl { ty; name; expr } ->
    let var, st' = declare_var st name in
    let ty = elab_ty st ty in
    let stmts =
      match expr with
      | None -> empty +> Ast.[ Declare { ty; var } ]
      | Some expr ->
        let tmp = fresh_var st "tmp" in
        let expr = elab_expr st expr in
        empty
        +> Ast.
             [ Declare { ty; var = tmp }
             ; Assign { lvalue = tmp; expr }
             ; Declare { ty; var }
             ; Assign { lvalue = var; expr = Var { var = tmp; ty = None } }
             ]
    in
    stmts, st'
  | Block block -> (empty +> Ast.[ Block (elab_block st block) ]), st
  | Post { lvalue; op } ->
    let lvalue = elab_var st lvalue in
    let expr =
      match op with
      | Incr -> Ast.Int_const 1L
      | Decr -> Ast.Int_const (-1L)
    in
    let stmts =
      empty
      +> Ast.
           [ Assign
               { lvalue
               ; expr =
                   Bin
                     { lhs = Var { var = lvalue; ty = None }
                     ; op = Add
                     ; rhs = expr
                     ; ty = None
                     }
               }
           ]
    in
    stmts, st
  | Assign { lvalue; op; expr } ->
    let expr = elab_expr st expr in
    let lvalue = elab_var st lvalue in
    let bin_expr op =
      Ast.(Bin { lhs = Var { var = lvalue; ty = None }; op; rhs = expr; ty = None })
    in
    let expr =
      match op with
      | Eq -> expr
      | Add_eq -> bin_expr Add
      | Sub_eq -> bin_expr Sub
      | Mul_eq -> bin_expr Mul
      | Div_eq -> bin_expr Div
      | Mod_eq -> bin_expr Mod
    in
    (empty +> Ast.[ Assign { lvalue; expr } ]), st
  | Return expr ->
    let expr = elab_expr st expr in
    (empty +> Ast.[ Return expr ]), st
  | If { cond; body1; body2 } ->
    let cond = elab_expr st cond in
    let body1 = elab_stmt_to_block st body1 in
    let body2 = body2 |> Option.map ~f:(elab_stmt_to_block st) in
    (empty +> Ast.[ If { cond; body1; body2 } ]), st
  | While { cond; body } ->
    let cond = elab_expr st cond in
    let body = elab_stmt_to_block st body in
    (empty +> Ast.[ While { cond; body } ]), st
  | For { paren = { init; cond; incr }; body } ->
    let init_stmts, init_st = elab_stmt st init in
    let cond = elab_expr init_st cond in
    let body = elab_stmt_to_block init_st body in
    let incr = elab_stmt_to_block init_st incr in
    let while_stmt = Ast.(While { cond; body = Block [ body; incr ] }) in
    let res = Ast.(Block (Bag.to_list (empty ++ init_stmts +> [ while_stmt ]))) in
    empty +> [ res ], st

and elab_stmt_to_block st (stmt : Cst.stmt) : Ast.stmt =
  let res, _ = elab_stmt st stmt in
  Block (Bag.to_list res)

and elab_expr st (expr : Cst.expr) : Ast.expr =
  match expr with
  | Int_const i ->
    Z.to_int64 i
    |> Option.value_or_thunk ~default:(fun () ->
      throw_s [%message "Int did not fit in 64 bits" (i : Z.t)])
    |> Int_const
  | Bool_const b -> Bool_const b
  | Var var ->
    let var = elab_var st var in
    Var { var; ty = None }
  | Neg expr ->
    let expr = elab_expr st expr in
    Bin { lhs = Int_const 0L; op = Sub; rhs = expr; ty = None }
  | Bin { lhs; op; rhs } ->
    let lhs = elab_expr st lhs in
    let rhs = elab_expr st rhs in
    let op = elab_bin_op op in
    Bin { lhs; op; rhs; ty = None }

and elab_bin_op (op : Cst.bin_op) : Ast.bin_op =
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

and elab_block st (block : Cst.block) : Ast.block =
  let rec go st stmts =
    match stmts with
    | [] -> Bag.empty
    | stmt :: stmts ->
      let stmt, st' = elab_stmt st stmt in
      let stmts = go st' stmts in
      stmt ++ stmts
  in
  go st block.stmts |> Bag.to_list
;;

let elab_program st (prog : Cst.program) : Ast.program =
  let ty = elab_ty st prog.ty in
  let name = prog.name in
  let block = elab_block st prog.block in
  { ty; name; block }
;;

let elaborate_program prog =
  let st = create_state () in
  match elab_program st prog with
  | exception Exn s -> error_s s
  | res -> Ok res
;;
