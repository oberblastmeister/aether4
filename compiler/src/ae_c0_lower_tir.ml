(* TODO: maybe use effects for the spans *)
open Std
module Label = Ae_label
module Ast = Ae_c0_ast
module Tir = Ae_tir_types
module Temp = Tir.Temp
module Bag = Ae_data_bag
module Span = Ae_span
open Bag.Syntax
open Ae_trace

let empty = Bag.empty
let ins ?ann ?info instr = Second (Tir.Instr'.create_unindexed ?ann ?info instr)
let label ?(params = []) l = empty +> [ First l; ins (Block_params params) ]
let bc ?(args = []) label = { Tir.Block_call.label; args }

type struct_info =
  { lowered_ty : Tir.Struct.t
  ; field_name_to_index : int String.Table.t
  ; strukt : Ast.strukt
  }

type global_st =
  { func_ty_map : (Ast.var, Ast.func_sig) Hashtbl.t
  ; typedefs : Ast.ty Ast.Var.Table.t
  ; structs : struct_info Ast.Var.Table.t
  }

type instrs = Tir.Linearized.instr Bag.t [@@deriving sexp_of]

let create_global_state _program =
  let func_ty_map = Hashtbl.create (module Ast.Var) in
  let typedefs = Ast.Var.Table.create () in
  let structs = Ast.Var.Table.create () in
  { func_ty_map; typedefs; structs }
;;

type st =
  { temp_gen : Temp.Id_gen.t
  ; var_to_temp : Temp.t Ast.Var.Table.t
  ; label_gen : Label.Id_gen.t
  ; global_st : global_st
  }

let create_state global_st =
  { temp_gen = Temp.Id_gen.create 0
  ; var_to_temp = Ast.Var.Table.create ()
  ; label_gen = Label.Id_gen.create 0
  ; global_st
  }
;;

let var_temp t var =
  Hashtbl.find_or_add t.var_to_temp var ~default:(fun () ->
    let id = Temp.Id_gen.get t.temp_gen in
    let temp = Temp.create ~info:(Span.to_info var.span) var.name id in
    temp)
;;

let fresh_temp ?(name = "fresh") ?info t : Temp.t =
  let id = Temp.Id_gen.get t.temp_gen in
  Temp.create ?info name id
;;

let fresh_label ?(name = "fresh") ?info t : Label.t =
  let id = Label.Id_gen.get t.label_gen in
  Label.create ?info name id
;;

let rec lower_ty st (ty : Ast.ty) : Tir.Ty.t =
  match ty with
  | Int _ -> Int
  | Bool _ -> Bool
  | Void _ -> Void
  | Ty_var var -> lower_ty st (Hashtbl.find_exn st.typedefs var)
  | Pointer { ty; span = _ } ->
    let ty = lower_ty st ty in
    Pointer ty
  | Ty_struct { name; span = _ } ->
    let struct_info = Hashtbl.find_exn st.structs name in
    Struct struct_info.lowered_ty
;;

let rec lower_block st (block : Ast.block) : instrs =
  List.map block ~f:(lower_stmt st) |> Bag.concat

and make_cond st cond body1 body2 =
  let then_label = fresh_label ~name:"then" st in
  let else_label = fresh_label ~name:"else" st in
  let join_label = fresh_label ~name:"join" st in
  empty
  +> [ ins (Cond_jump { cond; b1 = bc then_label; b2 = bc else_label }) ]
  ++ label then_label
  ++ body1
  +> [ ins (Jump (bc join_label)) ]
  ++ label else_label
  ++ body2
  +> [ ins (Jump (bc join_label)) ]
  ++ label join_label

(* returns the address of the lvalue *)
and lower_lvalue_address st dst (lvalue : Ast.expr) =
  match lvalue with
  | Deref { expr; span = _; ty } ->
    let ty = Option.value_exn ty |> lower_ty st.global_st in
    let cond = fresh_temp ~name:"cond" st in
    let garbage_temp = fresh_temp ~name:"garbage" st in
    let body1 =
      empty
      +> [ ins
             (Nary { dst = garbage_temp; op = C0_runtime_null_pointer_panic; srcs = [] })
         ; ins Unreachable
         ]
    in
    let body2 = empty in
    empty
    ++ lower_expr st dst expr
    +> [ ins (Unary { dst = cond; op = Is_null ty; src = dst }) ]
    ++ make_cond st cond body1 body2
  | Field_access { expr; field; span = _; ty = _res_ty } ->
    let%fail_exn (Ty_struct { name; _ }) = Ast.expr_ty_exn expr in
    let struct_info = Hashtbl.find_exn st.global_st.structs name in
    let struct_ty = struct_info.lowered_ty in
    let field = Hashtbl.find_exn struct_info.field_name_to_index field.t in
    let address = fresh_temp ~name:"address" st in
    (* need to check that the pointer is not null *)
    empty
    ++ lower_lvalue_address st address expr
    +> [ ins (Unary { dst; src = address; op = Offset_of { ty = struct_ty; field } }) ]
  | _ ->
    raise_s
      [%message
        "Should not get here, lvalues addresses should be a specific syntactic class"
          (lvalue : Ast.expr)]

(* invariant:

  `lower_stmt st cont stmt`
  
  lowers to instructions that will execute the continuation cont after stmt.
  
  This is essentially modeled like a cps transform.
  
  This means that we *MUST* use the cont variable!
*)
and lower_stmt st (stmt : Ast.stmt) : instrs =
  match stmt with
  | Declare { ty = _; var; span = _ } ->
    (* this adds it into the map, but ignore the temp *)
    let _ = var_temp st var in
    empty
  | Assign { lvalue = Var { var; ty = _ }; expr; span = _ } ->
    let temp = var_temp st var in
    lower_expr st temp expr
  | Assign { lvalue; expr; span = _ } ->
    let ty = Ast.expr_ty_exn lvalue |> lower_ty st.global_st in
    let address = fresh_temp ~name:"address" st in
    let garbage_temp = fresh_temp ~name:"garbage" st in
    let expr_dst = fresh_temp ~name:"store_expr" st in
    empty
    ++ lower_lvalue_address st address lvalue
    ++ lower_expr st expr_dst expr
    +> [ ins (Bin { dst = garbage_temp; op = Store ty; src1 = address; src2 = expr_dst })
       ]
  | Block { block; span = _ } -> lower_block st block
  | Effect expr ->
    let dst = fresh_temp ~name:"effect" st in
    lower_expr st dst expr
  | Assert { expr; span } ->
    let garbage_dst = fresh_temp ~name:"assert_garbage_dst" st in
    let cond_dst = fresh_temp ~name:"assert_cond" st in
    let t1, t2, t3, t4 =
      ( fresh_temp ~name:"span_start_line" st
      , fresh_temp ~name:"span_start_col" st
      , fresh_temp ~name:"span_end_line" st
      , fresh_temp ~name:"span_end_col" st )
    in
    let int dst i = ins (Nullary { dst; op = Int_const (Int64.of_int i) }) in
    empty
    ++ lower_expr st cond_dst expr
    +> [ int t1 span.start.line
       ; int t2 span.start.col
       ; int t3 span.stop.line
       ; int t4 span.stop.col
       ; ins
           (Nary
              { dst = garbage_dst
              ; op = C0_runtime_assert
              ; srcs = [ cond_dst; t1; t2; t3; t4 ]
              })
       ]
  | Return { expr; span } ->
    let dst = fresh_temp ~name:"ret" st in
    begin
      match expr with
      | None ->
        let info = Span.to_info span in
        empty
        +> [ ins ~info (Nullary { dst; op = Void_const })
           ; ins ~info (Ret { src = dst; ty = Void })
           ]
      | Some expr ->
        let ty = Ast.expr_ty_exn expr in
        let info = Span.to_info span in
        let ty = lower_ty st.global_st ty in
        empty ++ lower_expr st dst expr +> [ ins ~info (Ret { src = dst; ty }) ]
    end
  | If { cond; body1; body2; span } ->
    let info = Span.to_info span in
    let cond_temp = fresh_temp ~info ~name:"cond" st in
    let body1 = lower_stmt st body1 in
    let body2 = Option.value_map body2 ~f:(lower_stmt st) ~default:empty in
    lower_expr st cond_temp cond ++ make_cond st cond_temp body1 body2
  | While { cond; body; span } ->
    let info = Span.to_info span in
    let done_label = fresh_label ~name:"done" st in
    let cond_temp = fresh_temp ~info ~name:"cond" st in
    let loop_label = fresh_label ~info ~name:"loop" st in
    let body_label = fresh_label ~name:"body" st in
    let body = lower_stmt st body in
    empty
    +> [ ins ~info (Jump (bc loop_label)) ]
    ++ label loop_label
    ++ lower_expr st cond_temp cond
    +> [ ins
           ~info
           (Cond_jump { cond = cond_temp; b1 = bc body_label; b2 = bc done_label })
       ]
    ++ label body_label
    ++ body
    +> [ ins ~info (Jump (bc loop_label)) ]
    ++ label done_label

and lower_bin_op (op : Ast.bin_op) : Tir.Bin_op.t =
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
  | Bit_and -> And
  | Bit_or -> Or
  | Bit_xor -> Xor
  | Eq -> raise_s [%message "don't know type of eq, lower before"]
  | Lshift -> Lshift
  | Rshift -> Rshift

(* invariant:

  `lower_expr st cont dst expr stmt`
  
  lowers to instructions that will execute the continuation cont after expr.
  
  This is essentially modeled like a cps transform.
  
  This means that we *MUST* use the cont variable!
*)
and lower_expr st (dst : Temp.t) (expr : Ast.expr) : instrs =
  match expr with
  | Ternary { cond; then_expr; else_expr; ty = _; span } ->
    let info = Span.to_info span in
    let cond_dst = fresh_temp ~info ~name:"cond" st in
    let body1 = lower_expr st dst then_expr in
    let body2 = lower_expr st dst else_expr in
    empty ++ lower_expr st cond_dst cond ++ make_cond st cond_dst body1 body2
  | Int_const { t = const; span } ->
    let info = Span.to_info span in
    empty +> [ ins ~info (Nullary { dst; op = Int_const const }) ]
  | Bool_const { t = const; span } ->
    let info = Span.to_info span in
    empty +> [ ins ~info (Nullary { dst; op = Bool_const const }) ]
  | Bin { lhs; op; rhs; ty = _; span } ->
    let info = Span.to_info span in
    let op : Tir.Bin_op.t =
      match op with
      | Eq ->
        let ty = lower_ty st.global_st (Ast.expr_ty_exn lhs) in
        Eq ty
      | _ -> lower_bin_op op
    in
    let src1 = fresh_temp ~info ~name:"lhs" st in
    let src2 = fresh_temp ~info ~name:"rhs" st in
    empty
    ++ lower_expr st src1 lhs
    ++ lower_expr st src2 rhs
    +> [ ins ~info (Bin { dst; src1; op; src2 }) ]
  | Var { var; ty } ->
    let src = var_temp st var in
    let ty = lower_ty st.global_st (Option.value_exn ty) in
    empty +> [ ins ~info:(Span.to_info var.span) (Unary { dst; op = Copy ty; src }) ]
  | Deref _ | Field_access _ ->
    let lvalue = expr in
    let address = fresh_temp ~name:"address" st in
    let ty = Ast.expr_ty_exn lvalue |> lower_ty st.global_st in
    empty
    ++ lower_lvalue_address st address lvalue
    +> [ ins (Unary { dst; op = Deref ty; src = address }) ]
  | Call { func; args; ty; span } ->
    let ty = Option.value_exn ty in
    let info = Span.to_info span in
    let func_sig = Hashtbl.find_exn st.global_st.func_ty_map func in
    let arg_temps =
      List.mapi args ~f:(fun i _arg ->
        fresh_temp st ~info ~name:("arg" ^ Int.to_string i))
    in
    let ty = lower_ty st.global_st ty in
    let args =
      List.zip_exn arg_temps args
      |> List.map ~f:(fun (arg_temp, arg_expr) -> lower_expr st arg_temp arg_expr)
      |> Bag.concat
    in
    empty
    ++ args
    +> [ ins
           (Call
              { dst
              ; ty
              ; func = func.name
              ; args =
                  List.zip_exn arg_temps func_sig.params
                  |> List.map ~f:(fun (arg, param) -> arg, lower_ty st.global_st param.ty)
              ; is_extern = func_sig.is_extern
              })
       ]
  | Nullary { op; span = _ } -> begin
    match op with
    | Alloc ty ->
      let ty = lower_ty st.global_st ty in
      empty +> [ ins (Nullary { dst; op = Alloc ty }) ]
  end
;;

let lower_func_defn st (defn : Ast.func_defn) : Tir.Func.t =
  let st = create_state st in
  let start_label = fresh_label ~name:"start" st in
  let params =
    List.map defn.params ~f:(fun param ->
      { Tir.Block_param.param = var_temp st param.var
      ; ty = lower_ty st.global_st param.ty
      })
  in
  let linearized =
    empty ++ label ~params start_label ++ lower_block st defn.body +> [ ins Unreachable ]
  in
  let blocks = Tir.Linearized.to_blocks_exn (Bag.to_list linearized) in
  let next_temp_id = Temp.Id_gen.get st.temp_gen in
  let next_label_id = Label.Id_gen.get st.label_gen in
  let func : Tir.Func.t =
    { name = defn.name.name; blocks; start = start_label; next_temp_id; next_label_id }
  in
  func
;;

let lower_global_decl st (decl : Ast.global_decl) : Tir.Func.t option =
  match decl with
  | Extern_func_defn { name; ty } ->
    Hashtbl.set st.func_ty_map ~key:name ~data:ty;
    None
  | Func_decl { name; ty } ->
    Hashtbl.set st.func_ty_map ~key:name ~data:ty;
    None
  | Func_defn defn ->
    Hashtbl.set st.func_ty_map ~key:defn.name ~data:(Ast.func_defn_to_ty defn);
    Some (lower_func_defn st defn)
  | Typedef { ty; name; span = _ } ->
    Hashtbl.add_exn st.typedefs ~key:name ~data:ty;
    None
  | Struct { name; strukt; span = _ } ->
    begin
      let@: strukt = Option.iter strukt in
      let lowered_ty =
        List.map strukt.fields ~f:(fun field -> lower_ty st field.ty) |> Tir.Struct.create
      in
      let field_name_to_index =
        List.mapi strukt.fields ~f:(fun i field -> field.name.t, i)
        |> String.Table.of_alist_exn
      in
      let struct_info = { lowered_ty; field_name_to_index; strukt } in
      Hashtbl.add_exn st.structs ~key:name ~data:struct_info
    end;
    None
;;

let lower_program (program : Ast.program) : Tir.Program.t =
  let st = create_global_state program in
  let funcs = List.filter_map ~f:(lower_global_decl st) program in
  { funcs }
;;
