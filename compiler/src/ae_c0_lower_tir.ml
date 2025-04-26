open Std
module Label = Ae_label
module Ast = Ae_c0_ast
module Tir = Ae_tir_types
module Temp = Tir.Temp
module Bag = Ae_data_bag
module Span = Ae_span
open Bag.Syntax

let empty = Bag.empty
let ins = Tir.Instr'.create_unindexed
let bc ?(args = []) label = { Tir.Block_call.label; args }

type global_st =
  { func_ty_map : (Ast.var, Ast.func_sig) Hashtbl.t
  ; typedefs : Ast.ty Ast.Var.Table.t
  }

type instrs = Tir.Instr'.t Bag.t [@@deriving sexp_of]

let create_global_state _program =
  let func_ty_map = Hashtbl.create (module Ast.Var) in
  let typedefs = Ast.Var.Table.create () in
  { func_ty_map; typedefs }
;;

type st =
  { temp_gen : Temp.Id_gen.t
  ; var_to_temp : Temp.t Ast.Var.Table.t
  ; label_gen : Label.Id_gen.t
  ; mutable blocks : Tir.Block.t list
  ; global_st : global_st
  }

let create_state global_st =
  { temp_gen = Temp.Id_gen.create 0
  ; var_to_temp = Ast.Var.Table.create ()
  ; label_gen = Label.Id_gen.create 0
  ; blocks = []
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

let add_block ?(params = []) ?info t label instrs =
  (*
     make sure to add an unreachable instruction at the end so that all blocks have a terminator
  *)
  let block =
    Tir.Block.create
      label
      (Bag.to_arrayp
         (empty
          +> [ ins ?info (Block_params params) ]
          ++ instrs
          +> [ ins ?info Unreachable ]))
  in
  t.blocks <- block :: t.blocks;
  ()
;;

let add_fresh_block ?name ?info ?params t instrs : Label.t =
  let label = fresh_label ?name ?info t in
  add_block ?info ?params t label instrs;
  label
;;

let rec lower_ty st (ty : Ast.ty) : Tir.Ty.t =
  match ty with
  | Int _ -> Int
  | Bool _ -> Bool
  | Void _ -> Void
  | Ty_var var -> lower_ty st (Hashtbl.find_exn st.global_st.typedefs var)
;;

let rec lower_block st (cont : instrs) (block : Ast.block) : instrs =
  List.fold_right block ~init:cont ~f:(fun stmt cont -> lower_stmt st cont stmt)

and add_cond_jump ?info st cont cond_temp body1 body2 =
  let join_label = add_fresh_block ?info ~name:"join" st cont in
  let body1_label =
    add_fresh_block
      ?info
      ~name:"then"
      st
      (body1 (empty +> [ ins ?info (Jump (bc join_label)) ]))
  in
  let body2_label =
    add_fresh_block
      ?info
      ~name:"else"
      st
      (body2 (empty +> [ ins ?info (Jump (bc join_label)) ]))
  in
  empty
  +> [ ins
         ?info
         (Cond_jump { cond = cond_temp; b1 = bc body1_label; b2 = bc body2_label })
     ]

(* invariant:

  `lower_stmt st cont stmt`
  
  lowers to instructions that will execute the continuation cont after stmt.
  
  This is essentially modeled like a cps transform.
  
  This means that we *MUST* use the cont variable!
*)
and lower_stmt st (cont : instrs) (stmt : Ast.stmt) : instrs =
  match stmt with
  | Declare { ty = _; var; span = _ } ->
    let _ = var_temp st var in
    empty ++ cont
  | Assign { lvalue; expr; span = _ } ->
    let temp = var_temp st lvalue in
    lower_expr st cont temp expr
  | Block { block; span = _ } -> lower_block st cont block
  | Effect expr ->
    let dst = fresh_temp ~name:"effect" st in
    lower_expr st cont dst expr
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
        (* important: we override the continuation here because nothing should be after a Ret *)
        let ty = lower_ty st ty in
        let cont = empty +> [ ins ~info (Ret { src = dst; ty }) ] in
        lower_expr st cont dst expr
    end
  | If { cond; body1; body2; span } ->
    let info = Span.to_info span in
    let cond_temp = fresh_temp ~info ~name:"cond" st in
    let body1 cont = lower_stmt st cont body1 in
    let body2 cont = Option.value_map body2 ~f:(lower_stmt st cont) ~default:cont in
    let cont = add_cond_jump ~info st cont cond_temp body1 body2 in
    lower_expr st cont cond_temp cond
  | While { cond; body; span } ->
    let info = Span.to_info span in
    let done_label = add_fresh_block ~info ~name:"done" st cont in
    let cond_temp = fresh_temp ~info ~name:"cond" st in
    let loop_label = fresh_label ~info ~name:"loop" st in
    let body = lower_stmt st (empty +> [ ins ~info (Jump (bc loop_label)) ]) body in
    let body_label = add_fresh_block ~info ~name:"body" st body in
    add_block
      ~info
      st
      loop_label
      (lower_expr
         st
         (empty
          +> [ ins
                 ~info
                 (Cond_jump { cond = cond_temp; b1 = bc body_label; b2 = bc done_label })
             ])
         cond_temp
         cond);
    empty +> [ ins ~info (Jump (bc loop_label)) ]

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
and lower_expr st (cont : instrs) (dst : Temp.t) (expr : Ast.expr) : instrs =
  match expr with
  | Ternary { cond; then_expr; else_expr; ty = _; span } ->
    let info = Span.to_info span in
    let cond_dst = fresh_temp ~info ~name:"cond" st in
    let cont =
      add_cond_jump
        ~info
        st
        cont
        cond_dst
        (fun cont -> lower_expr st cont dst then_expr)
        (fun cont -> lower_expr st cont dst else_expr)
    in
    let cont = lower_expr st cont cond_dst cond in
    cont
  | Int_const { t = const; span } ->
    let info = Span.to_info span in
    empty +> [ ins ~info (Nullary { dst; op = Int_const const }) ] ++ cont
  | Bool_const { t = const; span } ->
    let info = Span.to_info span in
    empty +> [ ins ~info (Nullary { dst; op = Bool_const const }) ] ++ cont
  | Bin { lhs; op; rhs; ty = _; span } ->
    let info = Span.to_info span in
    let op : Tir.Bin_op.t =
      match op with
      | Eq ->
        let ty = lower_ty st (Ast.expr_ty_exn lhs) in
        Eq ty
      | _ -> lower_bin_op op
    in
    let src1 = fresh_temp ~info ~name:"lhs" st in
    let src2 = fresh_temp ~info ~name:"rhs" st in
    let cont = empty +> [ ins ~info (Bin { dst; src1; op; src2 }) ] ++ cont in
    let cont = lower_expr st cont src2 rhs in
    let cont = lower_expr st cont src1 lhs in
    cont
  | Var { var; ty } ->
    let src = var_temp st var in
    let ty = lower_ty st (Option.value_exn ty) in
    empty
    +> [ ins ~info:(Span.to_info var.span) (Unary { dst; op = Copy ty; src }) ]
    ++ cont
  | Call { func; args; ty; span } ->
    let ty = Option.value_exn ty in
    let info = Span.to_info span in
    let func_sig = Hashtbl.find_exn st.global_st.func_ty_map func in
    let arg_temps =
      List.mapi args ~f:(fun i _arg ->
        fresh_temp st ~info ~name:("arg" ^ Int.to_string i))
    in
    let cont =
      let ty = lower_ty st ty in
      empty
      +> [ ins
             (Call
                { dst
                ; ty
                ; func = func.name
                ; args =
                    List.zip_exn arg_temps func_sig.params
                    |> List.map ~f:(fun (arg, param) -> arg, lower_ty st param.ty)
                ; is_extern = func_sig.is_extern
                })
         ]
      ++ cont
    in
    let cont =
      List.zip_exn arg_temps args
      |> List.fold_right ~init:cont ~f:(fun (arg_temp, arg_expr) cont ->
        lower_expr st cont arg_temp arg_expr)
    in
    cont
;;

let lower_func_defn st (defn : Ast.func_defn) : Tir.Func.t =
  let st = create_state st in
  let start_instrs = lower_block st empty defn.body in
  let start_label =
    let params =
      List.map defn.params ~f:(fun param ->
        { Tir.Block_param.param = var_temp st param.var; ty = lower_ty st param.ty })
    in
    add_fresh_block ~info:(Span.to_info defn.span) ~name:"start" ~params st start_instrs
  in
  let next_temp_id = Temp.Id_gen.get st.temp_gen in
  let next_label_id = Label.Id_gen.get st.label_gen in
  let blocks = st.blocks in
  let func : Tir.Func.t =
    { name = defn.name.name
    ; blocks = blocks |> List.map ~f:(fun b -> b.label, b) |> Label.Map.of_alist_exn
    ; start = start_label
    ; next_temp_id
    ; next_label_id
    }
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
    Hashtbl.set st.typedefs ~key:name ~data:ty;
    None
;;

let lower_program (program : Ast.program) : Tir.Program.t =
  let st = create_global_state program in
  let funcs = List.filter_map ~f:(lower_global_decl st) program in
  { funcs }
;;
