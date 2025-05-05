open Std
module Cst = Ae_c0_cst
module Ast = Ae_c0_ast
module Bag = Ae_data_bag
module Span = Ae_span
module Spanned = Ae_spanned
open Bag.Syntax

let empty = Bag.empty

exception Exn of Sexp.t

type st =
  { next_temp_id : int ref
  ; next_func_id : int ref
  ; next_type_id : int ref
  ; next_struct_id : int ref
  ; context : Ast.var String.Map.t
  ; func_context : Ast.var String.Map.t
  ; typedef_context : Ast.var String.Map.t
  ; struct_context : Ast.var String.Map.t
  }

let create_state () =
  let next_temp_id = ref 0 in
  let next_func_id = ref 0 in
  let next_type_id = ref 0 in
  let next_struct_id = ref 0 in
  let context = String.Map.empty in
  let func_context = String.Map.empty in
  let typedef_context = String.Map.empty in
  let struct_context = String.Map.empty in
  { next_temp_id
  ; next_func_id
  ; next_type_id
  ; next_struct_id
  ; context
  ; func_context
  ; typedef_context
  ; struct_context
  }
;;

let throw_s s = raise (Exn s)

let elab_var st (var : Cst.var) : Ast.var =
  Map.find st.context var.t
  |> Option.value_or_thunk ~default:(fun () ->
    throw_s [%message "Variable not found" (var : Cst.var)])
;;

(*
   TODO: we don't implement the implicit struct declarations.
  It makes the implementation more annoying because we have to thread the state around here.
*)
let elab_struct_var st (var : Cst.var) : Ast.var =
  Map.find st.struct_context var.t
  |> Option.value_or_thunk ~default:(fun () ->
    throw_s [%message "Struct variable not found" (var : Cst.var)])
;;

let elab_func_var st (func_var : Cst.var) : Ast.var =
  Map.find st.func_context func_var.t
  |> Option.value_or_thunk ~default:(fun () ->
    throw_s [%message "Function variable not found" (func_var : Cst.var)])
;;

let fresh_var st (var : Cst.var) : Ast.var =
  let id : int = !(st.next_temp_id) in
  st.next_temp_id := id + 1;
  { name = var.t; id; span = var.span }
;;

let fresh_func_var st (var : Cst.var) : Ast.var =
  let id : int = !(st.next_func_id) in
  st.next_func_id := id + 1;
  { name = var.t; id; span = var.span }
;;

let fresh_typedef_var st (var : Cst.var) : Ast.var =
  let id : int = !(st.next_type_id) in
  st.next_type_id := id + 1;
  { name = var.t; id; span = var.span }
;;

let fresh_struct_var st (var : Cst.var) : Ast.var =
  let id : int = !(st.next_struct_id) in
  st.next_struct_id := id + 1;
  { name = var.t; id; span = var.span }
;;

let declare_typedef_var st (var : Cst.var) : Ast.var * st =
  assert (Map.is_empty st.context);
  if Map.mem st.func_context var.t
  then
    throw_s [%message "Cannot declare typedef with same name as function" (var : Cst.var)];
  if Map.mem st.typedef_context var.t
  then
    throw_s
      [%message
        "Cannot declare typedef with same name as existing typedef" (var : Cst.var)];
  let var' = fresh_typedef_var st var in
  var', { st with typedef_context = Map.add_exn st.typedef_context ~key:var.t ~data:var' }
;;

let declare_var st (var : Cst.var) : Ast.var * st =
  if Map.mem st.typedef_context var.t
  then throw_s [%message "Cannot declare var with same name as typedef" (var : Cst.var)];
  if Map.mem st.context var.t
  then throw_s [%message "Var was already declared!" (var : Cst.var)];
  let var' = fresh_var st var in
  var', { st with context = Map.add_exn st.context ~key:var.t ~data:var' }
;;

let declare_struct_var st (var : Cst.var) : Ast.var * st =
  (* we can declare structs multiple times *)
  match Map.find st.struct_context var.t with
  | Some t -> t, st
  | None ->
    let var' = fresh_struct_var st var in
    var', { st with struct_context = Map.add_exn st.struct_context ~key:var.t ~data:var' }
;;

let declare_func_var st (var : Cst.var) : Ast.var * st =
  if Map.mem st.typedef_context var.t
  then throw_s [%message "Cannot declare func with same name as typedef" (var : Cst.var)];
  match Map.find st.func_context var.t with
  | None ->
    let var' = fresh_func_var st var in
    var', { st with func_context = Map.add_exn st.func_context ~key:var.t ~data:var' }
  | Some var -> var, st
;;

let rec elab_ty st (ty : Cst.ty) : Ast.ty =
  match ty with
  | Bool span -> Bool span
  | Int span -> Int span
  | Void span -> Void span
  | Ty_var var ->
    let var = elab_var st var in
    Ty_var var
  | Pointer { ty; span } ->
    let ty = elab_ty st ty in
    Pointer { ty; span }
  | Ty_struct { name; span } ->
    let name = elab_struct_var st name in
    Ty_struct { name; span }
;;

let rec elab_stmt st (stmt : Cst.stmt) : Ast.stmt Bag.t * st =
  match stmt with
  | Assert { expr; span } ->
    let expr = elab_expr st expr in
    empty +> [ Ast.Assert { expr; span } ], st
  | Decl { ty; names; expr; span } ->
    let st_ref = ref st in
    if List.length names > 1 && Option.is_some expr
    then throw_s [%message "Can't have multiple variable declarations with assignment"];
    let stmts =
      let@: name = List.map names in
      let st = !st_ref in
      (* TODO: this is wrong, should be doing this in the typechecker because there may be typedefs *)
      let var, st' = declare_var st name in
      let ty = elab_ty st' ty in
      let stmts =
        match expr with
        | None -> empty +> Ast.[ Declare { ty; var; span } ]
        | Some expr ->
          let tmp = fresh_var st { t = "tmp"; span = name.span } in
          let expr = elab_expr st expr in
          empty
          +> Ast.
               [ Declare { ty; var = tmp; span }
               ; Assign { lvalue = Var { var = tmp; ty = None }; expr; span }
               ; Declare { ty; var; span }
               ; Assign
                   { lvalue = Var { var; ty = None }
                   ; expr = Var { var = tmp; ty = None }
                   ; span
                   }
               ]
      in
      st_ref := st';
      stmts
    in
    Bag.concat stmts, !st_ref
  | Block { block; span } ->
    (empty +> Ast.[ Block { block = elab_block st block; span } ]), st
  | Post { lvalue; op; span } ->
    let lvalue = elab_expr st lvalue in
    let expr =
      match op with
      | Incr -> Ast.Int_const { t = 1L; span }
      | Decr -> Ast.Int_const { t = -1L; span }
    in
    let stmts =
      empty
      +> Ast.
           [ Assign
               { lvalue
               ; expr = Bin { lhs = lvalue; op = Add; rhs = expr; ty = None; span }
               ; span
               }
           ]
    in
    stmts, st
  | Effect e ->
    let e = elab_expr st e in
    (empty +> Ast.[ Effect e ]), st
  | Assign { lvalue; op; expr; span } ->
    let expr = elab_expr st expr in
    let lvalue = elab_expr st lvalue in
    let bin_expr op = Ast.(Bin { lhs = lvalue; op; rhs = expr; ty = None; span }) in
    let expr =
      match op with
      | Id_assign -> expr
      | Add_assign -> bin_expr Add
      | Sub_assign -> bin_expr Sub
      | Mul_assign -> bin_expr Mul
      | Div_assign -> bin_expr Div
      | Mod_assign -> bin_expr Mod
      | Bit_and_assign -> bin_expr Bit_and
      | Bit_or_assign -> bin_expr Bit_or
      | Bit_xor_assign -> bin_expr Bit_xor
      | Lshift_assign -> bin_expr Lshift
      | Rshift_assign -> bin_expr Rshift
    in
    (empty +> Ast.[ Assign { lvalue; expr; span } ]), st
  | Return { expr; span } ->
    let expr = Option.map ~f:(elab_expr st) expr in
    (empty +> Ast.[ Return { expr; span } ]), st
  | If { cond; body1; body2; span } ->
    let cond = elab_expr st cond in
    let body1 = elab_stmt_to_block st body1 in
    let body2 = body2 |> Option.map ~f:(elab_stmt_to_block st) in
    (empty +> Ast.[ If { cond; body1; body2; span } ]), st
  | While { cond; body; span } ->
    let cond = elab_expr st cond in
    let body = elab_stmt_to_block st body in
    (empty +> Ast.[ While { cond; body; span } ]), st
  | For { paren = { init; cond; incr }; body; span } ->
    let init_stmts, init_st =
      Option.value_map ~f:(elab_stmt st) ~default:(empty, st) init
    in
    let body_span = Cst.stmt_span body in
    let cond =
      Option.value_map
        ~default:(Ast.Bool_const { t = true; span })
        ~f:(elab_expr init_st)
        cond
    in
    let body = elab_stmt_to_block init_st body in
    let incr =
      Option.value_map
        ~f:(elab_stmt_to_block init_st)
        ~default:(Ast.nop_stmt Span.none)
        incr
    in
    let while_stmt =
      Ast.(
        While
          { cond
          ; body = Block { block = [ body; incr ]; span = body_span }
          ; span = body_span
          })
    in
    let res =
      Ast.(Block { block = Bag.to_list (empty ++ init_stmts +> [ while_stmt ]); span })
    in
    empty +> [ res ], st

and elab_stmt_to_block st (stmt : Cst.stmt) : Ast.stmt =
  let res, _ = elab_stmt st stmt in
  Block { block = Bag.to_list res; span = Cst.stmt_span stmt }

and elab_expr st (expr : Cst.expr) : Ast.expr =
  match expr with
  | Var var -> Var { var = elab_var st var; ty = None }
  | Deref { expr; span } -> Deref { expr = elab_expr st expr; span; ty = None }
  | Field_access { expr; field; span } ->
    Field_access { expr = elab_expr st expr; field; span; ty = None }
  | Int_const { t = i; span } ->
    (match Z.to_int64 i with
     | None ->
       if Z.(equal i (shift_left (of_int 1) 63))
       then (
         try Int_const { t = Z.to_int64_unsigned i; span } with
         | Z.Overflow ->
           raise_s [%message "Bug: unexpected overflow on integer" (i : Z.t)])
       else throw_s [%message "Int did not fit in 64 bits" (i : Z.t)]
     | Some i -> Int_const { t = i; span })
  | Bool_const { t = b; span } -> Bool_const { t = b; span }
  | Unary { op; expr; span } ->
    let expr = elab_expr st expr in
    begin
      match op with
      | Neg ->
        Bin { lhs = Int_const { t = 0L; span }; op = Sub; rhs = expr; ty = None; span }
      | Bit_not ->
        Bin
          { lhs = Int_const { t = -1L; span }; op = Bit_xor; rhs = expr; ty = None; span }
      | Log_not ->
        Bin { lhs = Bool_const { t = false; span }; op = Eq; rhs = expr; ty = None; span }
    end
  | Ternary { cond; then_expr; else_expr; span } ->
    let cond = elab_expr st cond in
    let then_expr = elab_expr st then_expr in
    let else_expr = elab_expr st else_expr in
    Ternary { cond; then_expr; else_expr; ty = None; span }
  | Bin { lhs; op = Neq; rhs; span } ->
    elab_expr st (Unary { op = Log_not; expr = Bin { lhs; op = Eq; rhs; span }; span })
  | Bin { lhs; op; rhs; span } ->
    let lhs = elab_expr st lhs in
    let rhs = elab_expr st rhs in
    (match op with
     | Log_and ->
       Ternary
         { cond = lhs
         ; then_expr = rhs
         ; else_expr = Bool_const { t = false; span }
         ; ty = None
         ; span
         }
     | Log_or ->
       Ternary
         { cond = lhs
         ; then_expr = Bool_const { t = true; span }
         ; else_expr = rhs
         ; ty = None
         ; span
         }
     | _ ->
       let op = elab_bin_op op in
       Bin { lhs; op; rhs; ty = None; span })
  | Call { func; args; span } ->
    if Map.mem st.context func.t
    then throw_s [%message "Cannot call local variable" (span : Span.t)];
    let func = elab_func_var st func in
    let args = List.map args ~f:(elab_expr st) in
    Call { func; args; span; ty = None }
  | Alloc { ty; span } ->
    let ty = elab_ty st ty in
    Nullary { op = Alloc ty; span }

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
  | Bit_and -> Bit_and
  | Bit_or -> Bit_or
  | Bit_xor -> Bit_xor
  | Log_and -> raise_s [%message "should have been elaborated to ternary"]
  | Log_or -> raise_s [%message "should have been elaborated to ternary"]
  | Eq -> Eq
  | Neq -> raise_s [%message "should have been elaborated to not with eq"]
  | Lshift -> Lshift
  | Rshift -> Rshift

and elab_block st (block : Cst.stmt list) : Ast.block =
  let rec go st stmts =
    match stmts with
    | [] -> Bag.empty
    | stmt :: stmts ->
      let stmt, st' = elab_stmt st stmt in
      let stmts = go st' stmts in
      stmt ++ stmts
  in
  go st block |> Bag.to_list
;;

let elab_decl_param st (param : Cst.param) : Ast.param =
  let var = fresh_var st param.var in
  (match param.ty with
   | Void span ->
     throw_s [%message "Function parameters cannot have type void" (span : Span.t)]
   | _ -> ());
  let ty = elab_ty st param.ty in
  { var; ty; span = param.span }
;;

let elab_decl_param st (func : Cst.func) : Ast.func_sig =
  let ty = elab_ty st func.ty in
  let params = List.map ~f:(elab_decl_param st) func.params in
  { ty; params; span = func.span; is_extern = false }
;;

let elab_defn_param st (param : Cst.param) : Ast.param * st =
  let var, st' = declare_var st param.var in
  (match param.ty with
   | Void span ->
     throw_s [%message "Function parameters cannot have type void" (span : Span.t)]
   | _ -> ());
  let ty = elab_ty st' param.ty in
  { var; ty; span = param.span }, st'
;;

let elab_defn_params st params =
  let st = ref st in
  let res =
    List.map params ~f:(fun param ->
      let param, st' = elab_defn_param !st param in
      st := st';
      param)
  in
  res, !st
;;

let elab_field st (field : Cst.field) : Ast.field =
  let ty = elab_ty st field.ty in
  { ty; name = field.name; span = field.span }
;;

let elab_struct st (strukt : Cst.strukt) : Ast.strukt =
  let fields = List.map strukt.fields ~f:(elab_field st) in
  let fields_map =
    List.map fields ~f:(fun field -> field.name.t, field) |> String.Map.of_alist
  in
  let field_map =
    match fields_map with
    | `Duplicate_key field -> throw_s [%message "Duplicate struct field" (field : string)]
    | `Ok t -> t
  in
  ({ field_map; fields; span = strukt.span } : Ast.strukt)
;;

let elab_global_decl st (decl : Cst.global_decl) : Ast.global_decl * st =
  match decl with
  | Cst.Func func ->
    if func.is_extern
    then begin
      if Option.is_some func.body
      then throw_s [%message "Extern function must have no body"];
      let name, st' = declare_func_var st func.name in
      let func_sig = elab_decl_param st' func in
      Extern_func_defn { name; ty = { func_sig with is_extern = true } }, st'
    end
    else begin
      match func.body with
      | None ->
        let name, st' = declare_func_var st func.name in
        let func_sig = elab_decl_param st' func in
        Func_decl { name; ty = func_sig }, st
      | Some body ->
        let ty = elab_ty st func.ty in
        let name, st' = declare_func_var st func.name in
        let params, st_with_params = elab_defn_params st' func.params in
        let body = elab_block st_with_params body.block in
        Func_defn { ty; name; params; body; span = func.span }, st'
    end
  | Cst.Typedef typedef ->
    let ty = elab_ty st typedef.ty in
    let name, st = declare_var st typedef.name in
    Ast.Typedef { ty; name; span = typedef.span }, st
  | Struct { name; strukt; span } ->
    let name, st = declare_struct_var st name in
    let strukt = Option.map strukt ~f:(elab_struct st) in
    Struct { name; strukt; span }, st
;;

let elab_program st (prog : Cst.program) : Ast.program =
  let st_ref = ref st in
  let res =
    List.map prog ~f:(fun decl ->
      let decl, st' = elab_global_decl !st_ref decl in
      st_ref := st';
      decl)
  in
  let main_var, st = declare_func_var !st_ref { t = "main"; span = Span.none } in
  st_ref := st;
  st_ref := st;
  let main_decl =
    Ast.Func_decl
      { name = main_var
      ; ty = { ty = Ast.void_ty; params = []; span = Span.none; is_extern = false }
      }
  in
  main_decl :: res
;;

let elaborate_program prog =
  let st = create_state () in
  match elab_program st prog with
  | exception Exn s -> error_s s
  | res -> Ok res
;;
