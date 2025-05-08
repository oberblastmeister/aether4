(* TODO: maybe use effects for the spans *)
open Std
module Label = Ae_label
module Ast = Ae_c0_ast
module Tir = Ae_tir_types
module Temp = Tir.Temp
module Bag = Ae_data_bag
module Span = Ae_span
module Struct_layout = Ae_struct_layout
open Bag.Syntax
open Ae_trace

module Api = struct
  let empty = Bag.empty
  let ins ?ann ?info instr = Second (Tir.Instr'.create_unindexed ?ann ?info instr)
  let label ?(params = []) l = empty +> [ First l; ins (Block_params params) ]
  let bc ?(args = []) label = { Tir.Block_call.label; args }
  let const_int ?ann ?info dst i = ins ?ann ?info (Nullary { dst; op = Int_const i })
  let const_bool ?ann ?info dst b = ins ?ann ?info (Nullary { dst; op = Bool_const b })
  let const_null ?ann ?info dst = ins ?ann ?info (Nullary { dst; op = Null_ptr })
  let const_void ?ann ?info dst = ins ?ann ?info (Nullary { dst; op = Void_const })

  open struct
    let bin (op : Tir.Bin_op.t) ?ann ?info dst src1 src2 =
      ins ?ann ?info (Bin { dst; op; src1; src2 })
    ;;

    let unary (op : Tir.Unary_op.t) ?ann ?info dst src =
      ins ?ann ?info (Unary { dst; op; src })
    ;;
  end

  let add = bin Add
  let mul = bin Mul
  let sub = bin Sub
  let div = bin Div
  let eq_ptr = bin (Eq Ptr)
  let eq_int = bin (Eq Int)
  let offset_ptr = bin Offset_ptr
  let lt = bin Lt
  let copy_int = unary (Copy Int)
  let load_int = unary (Deref Int)
  let nary ?ann ?info dst op srcs = ins ?ann ?info (Nary { dst; op; srcs })
  let unreachable = ins Unreachable
  let deref ?ann ?info dst src ty = ins ?ann ?info (Unary { dst; op = Deref ty; src })

  let call ~dst:(dst, ty) ~is_extern func args =
    ins (Call { dst; ty; func; args; is_extern })
  ;;
end

open Api

type struct_info =
  { field_name_to_index : int String.Table.t
  ; strukt : Ast.strukt
  }

type global_st =
  { func_ty_map : (Ast.var, Ast.func_sig) Hashtbl.t
  ; structs : struct_info Ast.Var.Table.t
  ; struct_layouts : Struct_layout.t Ast.Var.Table.t
  }

type instrs = Tir.Linearized.instr Bag.t [@@deriving sexp_of]

let create_global_state _program =
  let func_ty_map = Hashtbl.create (module Ast.Var) in
  let structs = Ast.Var.Table.create () in
  let struct_layouts = Ast.Var.Table.create () in
  { func_ty_map; structs; struct_layouts }
;;

type st =
  { temp_gen : Temp.Id_gen.t
  ; var_to_temp : Temp.t Ast.Var.Table.t
  ; label_table : Tir.Label.t Ast.Var.Table.t
  ; label_gen : Label.Id_gen.t
  ; global_st : global_st
  }

let create_state global_st =
  { temp_gen = Temp.Id_gen.create 0
  ; var_to_temp = Ast.Var.Table.create ()
  ; label_table = Ast.Var.Table.create ()
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

let get_label t label =
  Hashtbl.find_or_add t.label_table label ~default:(fun () ->
    let id = Label.Id_gen.get t.label_gen in
    let label = Label.create ~info:(Span.to_info label.span) label.name id in
    label)
;;

let fresh_temp ?(name = "fresh") ?info t : Temp.t =
  let id = Temp.Id_gen.get t.temp_gen in
  Temp.create ?info name id
;;

let fresh_label ?(name = "fresh") ?info t : Label.t =
  let id = Label.Id_gen.get t.label_gen in
  Label.create ?info name id
;;

let rec lower_ty ty =
  let rec go (ty : Ast.ty) =
    match ty with
    | Int _ -> `Small Tir.Ty.Int
    | Bool _ -> `Small Bool
    | Void _ -> `Small Void
    | Ty_var var ->
      raise_s
        [%message
          "Should have unfolded all typedefs in the type checker" (var : Ast.Var.t)]
    | Pointer _ -> `Small Ptr
    | Ty_struct { name; _ } -> `Large name
    | Array _ -> `Small Ptr
    | Any ->
      raise_s
        [%message
          "If an expression has type Any then it should not have gotten past the type \
           checker"]
  in
  go ty

and lower_small_ty_exn ty =
  match lower_ty ty with
  | `Small ty -> ty
  | `Large _ -> raise_s [%message "Expected small type" (ty : Ast.ty)]

(*
   These functions could infinitely recurse because structs can be uninhabited when they mutually recurse.
   For example,
   
   struct bad2;
   struct bad1 {
      struct bad2 bad;
   };
   struct bad2 {
      struct bad1 bad;
   };
   
   These structures are uninhabited, because they reference each other.
   They also have an unbounded size.
   So it is crucial that we check that structs are not cyclic in the semantic analyzer.
   
   TODO: maybe add a already visited check here just to be extra sure it terminates.
*)
and size_of st (ty : Ast.ty) : int =
  match lower_ty ty with
  | `Small ty -> Tir.Ty.size_of ty
  | `Large name ->
    let layout = calculate_struct_layout st name in
    layout.size

and align_of st (ty : Ast.ty) : int =
  match lower_ty ty with
  | `Small ty -> Tir.Ty.align_of ty
  | `Large name ->
    let layout = calculate_struct_layout st name in
    layout.align

and calculate_struct_layout st (name : Ast.var) : Struct_layout.t =
  (* memoize this calculation *)
  Hashtbl.find_or_add st.struct_layouts name ~default:(fun () ->
    let struct_info = Hashtbl.find_exn st.structs name in
    List.map struct_info.strukt.fields ~f:(fun field ->
      let size = size_of st field.ty in
      let align = align_of st field.ty in
      Struct_layout.Field.{ size; align })
    |> Struct_layout.calculate)
;;

let make_cond st cond body1 body2 =
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
;;

let check_not_null st temp =
  let cond = fresh_temp ~name:"cond" st in
  let garbage_temp = fresh_temp ~name:"garbage" st in
  let null = fresh_temp ~name:"null" st in
  empty
  +> [ const_null null; eq_ptr cond temp null ]
  ++ make_cond
       st
       cond
       (empty +> [ nary garbage_temp C0_runtime_null_pointer_panic []; ins Unreachable ])
       empty
;;

let check_bounds st ~array ~index =
  let cond = fresh_temp ~name:"cond" st in
  let len = fresh_temp ~name:"len" st in
  let garbage_temp = fresh_temp ~name:"garbage" st in
  empty
  +> [ load_int len array; lt cond index len ]
  ++ make_cond
       st
       cond
       empty
       (empty
        +> [ call
               ~dst:(garbage_temp, Void)
               ~is_extern:true
               "c0_runtime_out_of_bounds_panic"
               []
           ; unreachable
           ])
;;

let rec lower_block st (block : Ast.block) : instrs =
  let instrs = List.map block.stmts ~f:(lower_stmt st) |> Bag.concat in
  let label =
    Option.map
      ~f:(fun label ->
        let label = get_label st label in
        (* TODO: allow implicit fallthrough for linearized *)
        empty +> [ ins (Jump (bc label)) ] ++ Api.label label)
      block.label
    |> Option.to_list
    |> Bag.concat
  in
  instrs ++ label

(* returns the address of the lvalue *)
and lower_lvalue_address st dst (lvalue : Ast.expr) =
  match lvalue with
  | Deref { expr; span = _; ty = _ } ->
    empty ++ lower_expr st dst expr ++ check_not_null st dst
  | Field_access { expr; field; span = _; ty = _res_ty } ->
    (* TODO: issue here with evaluating ty. Let's just unfold all types in the elaborator *)
    let name =
      match Ast.expr_ty_exn expr with
      | Ty_struct { name; _ } -> name
      | ty -> raise_s [%message "Expected struct ty" (ty : Ast.ty)]
    in
    let struct_info = Hashtbl.find_exn st.global_st.structs name in
    let field_index = Hashtbl.find_exn struct_info.field_name_to_index field.t in
    let layout = calculate_struct_layout st.global_st name in
    let offset = layout.offsets.@(field_index) in
    let offset_temp = fresh_temp ~name:"offset" st in
    let address = fresh_temp ~name:"address" st in
    empty
    ++ lower_lvalue_address st address expr
    +> [ const_int offset_temp (Int64.of_int offset); offset_ptr dst address offset_temp ]
  | Index { expr; index; span = _; ty = _ } ->
    let%fail_exn (Array { ty = inner_ty; span = _ }) = Ast.expr_ty_exn expr in
    let size = size_of st.global_st inner_ty |> Int64.of_int in
    let size_temp = fresh_temp ~name:"size" st in
    let array_temp = fresh_temp ~name:"array" st in
    let index_temp = fresh_temp ~name:"index" st in
    let offset_temp = fresh_temp ~name:"offset" st in
    let const_temp = fresh_temp ~name:"temp" st in
    empty
    ++ lower_expr st array_temp expr
    ++ check_not_null st array_temp
    ++ lower_expr st index_temp index
    ++ check_bounds st ~array:array_temp ~index:index_temp
    +> [ const_int size_temp size
       ; mul offset_temp size_temp index_temp
       ; const_int const_temp 8L
       ; add offset_temp offset_temp const_temp
       ; offset_ptr dst array_temp offset_temp
       ]
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
  | Break { label; span = _ } ->
    let label = get_label st label in
    empty +> [ ins (Jump (bc label)) ]
  | Declare { ty = _; var; span = _ } ->
    (* this adds it into the map, but ignore the temp *)
    let _ = var_temp st var in
    empty
  | Assign { lvalue = Var { var; ty = _ }; expr; span = _ } ->
    let temp = var_temp st var in
    lower_expr st temp expr
  | Assign { lvalue; expr; span = _ } ->
    let ty = Ast.expr_ty_exn lvalue |> lower_small_ty_exn in
    let address = fresh_temp ~name:"address" st in
    let garbage_temp = fresh_temp ~name:"garbage" st in
    let expr_dst = fresh_temp ~name:"store_expr" st in
    empty
    ++ lower_lvalue_address st address lvalue
    ++ lower_expr st expr_dst expr
    +> [ ins (Bin { dst = garbage_temp; op = Store ty; src1 = address; src2 = expr_dst })
       ]
  | Block block -> lower_block st block
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
    let const_int dst i = const_int dst (Int64.of_int i) in
    empty
    ++ lower_expr st cond_dst expr
    +> [ const_int t1 span.start.line
       ; const_int t2 span.start.col
       ; const_int t3 span.stop.line
       ; const_int t4 span.stop.col
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
        empty +> [ const_void dst; ins ~info (Ret { src = dst; ty = Void }) ]
      | Some expr ->
        let ty = Ast.expr_ty_exn expr in
        let info = Span.to_info span in
        let ty = lower_small_ty_exn ty in
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
  | Null { ty; _ } ->
    let ty = Option.value_exn ty in
    begin
      match lower_small_ty_exn ty with
      | Int -> empty +> [ const_int dst 0L ]
      | Bool -> empty +> [ const_bool dst false ]
      | Ptr -> empty +> [ const_null dst ]
      | Void -> empty +> [ const_void dst ]
    end
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
        let ty = lower_small_ty_exn (Ast.expr_ty_exn lhs) in
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
    let ty = lower_small_ty_exn (Option.value_exn ty) in
    empty +> [ ins ~info:(Span.to_info var.span) (Unary { dst; op = Copy ty; src }) ]
  | Deref _ | Field_access _ | Index _ ->
    let lvalue = expr in
    let address = fresh_temp ~name:"address" st in
    let ty = Ast.expr_ty_exn lvalue |> lower_small_ty_exn in
    empty ++ lower_lvalue_address st address lvalue +> [ deref dst address ty ]
  | Call { func; args; ty; span } ->
    let ty = Option.value_exn ty in
    let info = Span.to_info span in
    let func_sig = Hashtbl.find_exn st.global_st.func_ty_map func in
    let arg_temps =
      List.mapi args ~f:(fun i _arg ->
        fresh_temp st ~info ~name:("arg" ^ Int.to_string i))
    in
    let ty = lower_small_ty_exn ty in
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
                  |> List.map ~f:(fun (arg, param) -> arg, lower_small_ty_exn param.ty)
              ; is_extern = func_sig.is_extern
              })
       ]
  | Nullary { op; span = _; ty = _ } -> begin
    match op with
    | Alloc ty ->
      let size = size_of st.global_st ty in
      let align = align_of st.global_st ty in
      empty +> [ ins (Nullary { dst; op = Alloc { size; align } }) ]
  end
  | Alloc_array { arg_ty; expr; span = _; ty = _ } ->
    let amount = fresh_temp ~name:"amount" st in
    let size = size_of st.global_st arg_ty in
    let align = align_of st.global_st arg_ty in
    empty
    ++ lower_expr st amount expr
    +> [ ins (Unary { dst; op = Alloc_array { size; align }; src = amount }) ]
;;

let lower_func_defn st (defn : Ast.func_defn) : Tir.Func.t =
  let st = create_state st in
  let start_label = fresh_label ~name:"start" st in
  let params =
    List.map defn.ty.params ~f:(fun param ->
      { Tir.Block_param.param = var_temp st param.var; ty = lower_small_ty_exn param.ty })
  in
  let linearized =
    empty ++ label ~params start_label ++ lower_block st defn.body +> [ ins Unreachable ]
  in
  trace_ls [%lazy_message (linearized : Tir.Linearized.instr Bag.t)];
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
    Hashtbl.set st.func_ty_map ~key:defn.name ~data:defn.ty;
    Some (lower_func_defn st defn)
  | Typedef _ -> None
  | Struct { name; strukt; span = _ } ->
    begin
      let@: strukt = Option.iter strukt in
      let field_name_to_index =
        List.mapi strukt.fields ~f:(fun i field -> field.name.t, i)
        |> String.Table.of_alist_exn
      in
      let struct_info = { strukt; field_name_to_index } in
      Hashtbl.add_exn st.structs ~key:name ~data:struct_info
    end;
    None
;;

let lower_program (program : Ast.program) : Tir.Program.t =
  let st = create_global_state program in
  let funcs = List.filter_map ~f:(lower_global_decl st) program in
  { funcs }
;;
