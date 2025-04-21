open Std
module Ast = Ae_c0_ast
module Span = Ae_span

exception Exn of Sexp.t

type st =
  { context : Ast.ty Ast.Var.Map.t
  ; declared_funcs : Ast.func_sig Ast.Var.Map.t
  ; defined_funcs : Ast.Var.Set.t
  ; typedefs : Ast.ty Ast.Var.Map.t
  }

let create_state () =
  { context = Ast.Var.Map.empty
  ; declared_funcs = Ast.Var.Map.empty
  ; defined_funcs = Ast.Var.Set.empty
  ; typedefs = Ast.Var.Map.empty
  }
;;

let throw_s s = raise (Exn s)

let var_ty st var =
  Map.find st.context var
  |> Option.value_or_thunk ~default:(fun () ->
    throw_s [%message "Var not found!" (var : Ast.Var.t)])
;;

let check_ty_eq _st span (ty : Ast.ty) (ty' : Ast.ty) =
  match ty, ty' with
  | Int _, Int _ -> ()
  | Bool _, Bool _ -> ()
  | _ ->
    throw_s [%message "Types were not equal" (span : Span.t) (ty : Ast.ty) (ty' : Ast.ty)]
;;

let rec infer_expr st (expr : Ast.expr) : Ast.expr =
  match expr with
  | Var { var; ty = _ } -> Var { var; ty = Some (var_ty st var) }
  | Int_const _ | Bool_const _ -> expr
  | Ternary { cond; then_expr; else_expr; ty = _; span } ->
    let cond = check_expr st cond Ast.bool_ty in
    let then_expr = infer_expr st then_expr in
    let else_expr = infer_expr st else_expr in
    check_ty_eq st span (Ast.expr_ty_exn then_expr) (Ast.expr_ty_exn else_expr);
    Ternary { cond; then_expr; else_expr; ty = Some (Ast.expr_ty_exn then_expr); span }
  | Bin { lhs; op; rhs; ty = _; span } ->
    (match op with
     | Add | Sub | Mul | Div | Mod | Bit_and | Bit_or | Bit_xor | Lshift | Rshift ->
       let lhs = check_expr st lhs Ast.int_ty in
       let rhs = check_expr st rhs Ast.int_ty in
       Bin { lhs; op; rhs; ty = Some Ast.int_ty; span }
     | Lt | Gt | Le | Ge ->
       let lhs = check_expr st lhs Ast.int_ty in
       let rhs = check_expr st rhs Ast.int_ty in
       Bin { lhs; op; rhs; ty = Some Ast.bool_ty; span }
     | Eq ->
       let lhs = infer_expr st lhs in
       let rhs = infer_expr st rhs in
       check_ty_eq st span (Ast.expr_ty_exn lhs) (Ast.expr_ty_exn rhs);
       Bin { lhs; op; rhs; ty = Some Ast.bool_ty; span })
  | Call { func; args; ty = _; span } ->
    let func_sig = Map.find_exn st.declared_funcs func in
    let args, rem = List.zip_with_remainder args func_sig.params in
    if Option.is_some rem
    then
      throw_s
        [%message
          "Invalid number of arguments"
            ~expected:(List.length func_sig.params : int)
            ~actual:(List.length args : int)];
    let args = List.map args ~f:(fun (arg, param) -> check_expr st arg param.ty) in
    Call { func; args; ty = Some func_sig.ty; span }

and check_expr st (expr : Ast.expr) (ty : Ast.ty) : Ast.expr =
  let expr = infer_expr st expr in
  check_ty_eq st (Ast.expr_span expr) ty (Ast.expr_ty_exn expr);
  expr
;;

let rec check_stmt st (stmt : Ast.stmt) : Ast.stmt =
  match stmt with
  | If { cond; body1; body2; span } ->
    let cond = check_expr st cond Ast.bool_ty in
    let body1 = check_stmt st body1 in
    let body2 = Option.map body2 ~f:(check_stmt st) in
    If { cond; body1; body2; span }
  | Block { block; span } -> Block { block = check_block st block; span }
  | While { cond; body; span } ->
    let cond = check_expr st cond Ast.bool_ty in
    let body = check_stmt st body in
    While { cond; body; span }
  | Return { expr; span } ->
    (* TODO: fix this *)
    Return { expr = check_expr st expr Ast.int_ty; span }
  | Effect expr ->
    let expr = infer_expr st expr in
    Effect expr
  | Declare _ -> stmt
  | Assign { lvalue; expr; span } ->
    let ty = infer_expr st (Var { var = lvalue; ty = None }) |> Ast.expr_ty_exn in
    let expr = check_expr st expr ty in
    Assign { lvalue; expr; span }

and check_block st (block : Ast.block) : Ast.block =
  let rec loop st stmts =
    match stmts with
    | [] -> []
    | stmt :: stmts ->
      let stmt = check_stmt st stmt in
      let st' =
        match stmt with
        | Declare { ty; var; span = _ } ->
          { st with context = Map.add_exn st.context ~key:var ~data:ty }
        | _ -> st
      in
      let stmts = loop st' stmts in
      stmt :: stmts
  in
  loop st block
;;

let check_func_sig_eq st name (func_sig1 : Ast.func_sig) (func_sig2 : Ast.func_sig) =
  let params =
    match List.zip func_sig1.params func_sig2.params with
    | Ok t -> t
    | Unequal_lengths ->
      throw_s [%message "Parameters had unequal lengths" (name : Ast.var)]
  in
  begin
    let@: param1, param2 = List.iter params in
    check_ty_eq st Span.none param1.ty param2.ty
  end
;;

let declare_func st name (func_sig : Ast.func_sig) =
  match Map.find st.declared_funcs name with
  | None ->
    { st with declared_funcs = Map.set st.declared_funcs ~key:name ~data:func_sig }
  | Some func_sig' ->
    check_func_sig_eq st name func_sig' func_sig;
    st
;;

let define_func st name =
  if Set.mem st.defined_funcs name
  then throw_s [%message "Function defined multiple times!" (name : Ast.var)];
  { st with defined_funcs = Set.add st.defined_funcs name }
;;

let check_global_decl st (global_decl : Ast.global_decl) : Ast.global_decl * st =
  match global_decl with
  | Ast.Extern_func_defn { name; ty } ->
    let st = declare_func st name ty in
    let st = define_func st name in
    global_decl, st
  | Ast.Func_decl { name; ty } ->
    let st = declare_func st name ty in
    global_decl, st
  | Ast.Func_defn func ->
    let st = declare_func st func.name (Ast.func_defn_to_ty func) in
    let st = define_func st func.name in
    let st_with_params =
      List.fold func.params ~init:st ~f:(fun st param ->
        { st with context = Map.add_exn st.context ~key:param.var ~data:param.ty })
    in
    let body = check_block st_with_params func.body in
    let func = { func with body } in
    Ast.Func_defn func, st
  | Ast.Typedef typedef ->
    ( global_decl
    , { st with typedefs = Map.add_exn st.typedefs ~key:typedef.name ~data:typedef.ty } )
;;

let check_program st (prog : Ast.program) =
  let res, st =
    let st = ref st in
    let res =
      List.map prog ~f:(fun global_decl ->
        let global_decl, st' = check_global_decl !st global_decl in
        st := st';
        global_decl)
    in
    res, !st
  in
  Map.iter_keys st.declared_funcs ~f:(fun declared_func ->
    if not (Set.mem st.defined_funcs declared_func)
    then throw_s [%message "Declared function was not defined" (declared_func : Ast.var)]);
  res
;;

let check_program prog =
  let st = create_state () in
  match check_program st prog with
  | prog -> Ok prog
  | exception Exn s -> error_s s
;;
