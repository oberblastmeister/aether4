open Std
module Cst = Ae_c0_cst

exception Error of Error.t

let throw_s s = raise (Error (Error.create_s s))

type st =
  { declared : String.Set.t
  ; defined : String.Set.t
  ; found_return : bool
  }
[@@deriving sexp_of]

let rec check_expr st (expr : Cst.expr) =
  match expr with
  | Cst.IntConst i ->
    Z.to_int64 i
    |> Option.value_or_thunk ~default:(fun () ->
      throw_s [%message "the integer did not fit in 64 bits" (i : Z.t)])
    |> ignore;
    ()
  | Cst.Var var ->
    if st.found_return
    then ()
    else if Set.mem st.defined var
    then ()
    else throw_s [%message "Var was not initialized" (var : string)]
  | Cst.Neg e -> check_expr st e
  | Cst.Bin { lhs; op = _; rhs } ->
    check_expr st lhs;
    check_expr st rhs;
    ()
;;

let check_decl st (decl : Cst.decl) =
  match decl.expr with
  | None -> { st with declared = Set.add st.declared decl.name }
  | Some e ->
    check_expr st e;
    { st with
      declared = Set.add st.declared decl.name
    ; defined = Set.add st.defined decl.name
    }
;;

let rec check_stmt st (stmt : Cst.stmt) =
  match stmt with
  | Decl decl -> check_decl st decl
  | Block block -> check_block st block
  | Assign { lvalue; op; expr } when Set.mem st.declared lvalue ->
    check_expr st expr;
    (match op with
     | Eq -> { st with defined = Set.add st.defined lvalue }
     | _ when st.found_return -> st
     | _ when Set.mem st.defined lvalue -> st
     | _ -> throw_s [%message "Variable was not initialized!" (lvalue : string)])
  | Assign { lvalue; _ } -> throw_s [%message "Variable not declared!" (lvalue : string)]
  | Return e ->
    check_expr st e;
    { st with found_return = true }
  | _ -> todol [%here]

and check_block st (block : Cst.block) =
  let st' = List.fold_left ~init:st ~f:(fun st stmt -> check_stmt st stmt) block.stmts in
  let newly_declared = Set.diff st'.declared st.declared in
  let newly_defined = Set.diff st'.defined st.defined in
  let newly_defined = Set.diff newly_defined newly_declared in
  { st with
    defined = Set.union st.defined newly_defined
  ; found_return = st'.found_return
  }
;;

let check_program (prog : Cst.program) =
  let st =
    { declared = String.Set.empty; defined = String.Set.empty; found_return = false }
  in
  let st' = check_block st prog.block in
  if not st'.found_return then throw_s [%message "Did not find return statement"]
;;

let check_program prog =
  match check_program prog with
  | exception Error e -> Result.Error e
  | _st -> Result.Ok ()
;;
