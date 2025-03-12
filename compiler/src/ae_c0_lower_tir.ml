open Std
module Label_entity = Ae_label_entity
module Label = Label_entity.Ident
open Ae_trace
module Ast = Ae_c0_ast
module Tir = Ae_tir_types
module Entity = Ae_entity_std
module Temp = Tir.Temp
module Id_gen = Entity.Id_gen
module Bag = Ae_data_bag
open Bag.Syntax

let empty = Bag.empty
let ins = Tir.Instr'.create_unindexed
let bc = Tir.Block_call.create

type st =
  { temp_gen : Tir.Temp_entity.Witness.t Id_gen.t
  ; var_to_temp : Temp.t Ast.Var.Table.t
  ; label_gen : Tir.Label_entity.Witness.t Id_gen.t
  ; mutable blocks : Tir.Block.t list
  }

type instrs = Tir.Instr'.t Bag.t [@@deriving sexp_of]

let create_state () =
  { temp_gen = Id_gen.create ()
  ; var_to_temp = Ast.Var.Table.create ()
  ; label_gen = Id_gen.create ()
  ; blocks = []
  }
;;

let var_temp t var =
  Hashtbl.find_or_add t.var_to_temp var ~default:(fun () ->
    let id = Id_gen.next t.temp_gen in
    let temp = { Entity.Ident.id; name = var.name } in
    temp)
;;

let fresh_temp ?(name = "fresh") t : Temp.t =
  let id = Id_gen.next t.temp_gen in
  Entity.Ident.create name id
;;

let fresh_label ?(name = "fresh") t : Label.t =
  let id = Id_gen.next t.label_gen in
  Entity.Ident.create name id
;;

let add_block t label instrs =
  (*
     make sure to add an unreachable instruction at the end so that all blocks have a terminator
  *)
  let block =
    Tir.Block.create
      label
      (Bag.to_arrayp
         (empty +> [ ins (Block_params { temps = [] }) ] ++ instrs +> [ ins Unreachable ]))
  in
  t.blocks <- block :: t.blocks;
  ()
;;

let add_fresh_block ?name t instrs : Label.t =
  let label = fresh_label ?name t in
  add_block t label instrs;
  label
;;

let lower_ty (ty : Ast.ty) : Tir.Ty.t =
  match ty with
  | Int -> Int
  | Bool -> Bool
;;

let rec lower_block st (cont : instrs) (block : Ast.block) : instrs =
  List.fold_right block ~init:cont ~f:(fun stmt cont -> lower_stmt st cont stmt)

and add_cond_jump st cont cond_temp body1 body2 =
  let join_label = add_fresh_block ~name:"join" st cont in
  let body1_label =
    add_fresh_block ~name:"then" st (body1 (empty +> [ ins (Jump (bc join_label)) ]))
  in
  let body2_label =
    add_fresh_block ~name:"else" st (body2 (empty +> [ ins (Jump (bc join_label)) ]))
  in
  empty
  +> [ ins (Cond_jump { cond = cond_temp; b1 = bc body1_label; b2 = bc body2_label }) ]

(* invariant:

  `lower_stmt st cont stmt`
  
  lowers to instructions that will execute the continuation cont after stmt.
  
  This is essentially modeled like a cps transform.
  
  This means that we *MUST* use the cont variable!
*)
and lower_stmt st (cont : instrs) (stmt : Ast.stmt) : instrs =
  trace_s [%message "lower_stmt" (stmt : Ast.stmt)];
  match stmt with
  | Declare { ty = _; var } ->
    let _ = var_temp st var in
    empty ++ cont
  | Assign { lvalue; expr } ->
    let temp = var_temp st lvalue in
    lower_expr st cont temp expr
  | Block block -> lower_block st cont block
  | Effect expr ->
    let dst = fresh_temp ~name:"effect" st in
    lower_expr st cont dst expr
  | Return expr ->
    let dst = fresh_temp ~name:"ret" st in
    let ty = Ast.expr_ty_exn expr in
    (* important: we override the continuation here because nothing should be after a Ret *)
    let cont = empty +> [ ins (Ret { src = dst; ty = lower_ty ty }) ] in
    lower_expr st cont dst expr
  | If { cond; body1; body2 } ->
    let cond_temp = fresh_temp ~name:"cond" st in
    let body1 cont = lower_stmt st cont body1 in
    let body2 cont = Option.value_map body2 ~f:(lower_stmt st cont) ~default:cont in
    let cont = add_cond_jump st cont cond_temp body1 body2 in
    lower_expr st cont cond_temp cond
  | While { cond; body } ->
    let done_label = add_fresh_block ~name:"done" st cont in
    let cond_temp = fresh_temp ~name:"cond" st in
    let loop_label = fresh_label ~name:"loop" st in
    let body = lower_stmt st (empty +> [ ins (Jump (bc loop_label)) ]) body in
    let body_label = add_fresh_block ~name:"body" st body in
    add_block
      st
      loop_label
      (lower_expr
         st
         (empty
          +> [ ins
                 (Cond_jump { cond = cond_temp; b1 = bc body_label; b2 = bc done_label })
             ])
         cond_temp
         cond);
    empty +> [ ins (Jump (bc loop_label)) ]

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
  | Ternary { cond; then_expr; else_expr; ty = _ } ->
    let cond_dst = fresh_temp ~name:"cond" st in
    let cont =
      add_cond_jump
        st
        cont
        cond_dst
        (fun cont -> lower_expr st cont dst then_expr)
        (fun cont -> lower_expr st cont dst else_expr)
    in
    let cont = lower_expr st cont cond_dst cond in
    cont
  | Int_const const -> empty +> [ ins (Nullary { dst; op = Int_const const }) ] ++ cont
  | Bool_const const -> empty +> [ ins (Nullary { dst; op = Bool_const const }) ] ++ cont
  | Bin { lhs; op; rhs; ty = _ } ->
    let op : Tir.Bin_op.t =
      match op with
      | Eq -> Eq (lower_ty (Ast.expr_ty_exn lhs))
      | _ -> lower_bin_op op
    in
    let src1 = fresh_temp ~name:"lhs" st in
    let src2 = fresh_temp ~name:"rhs" st in
    let cont = empty +> [ ins (Bin { dst; src1; op; src2 }) ] ++ cont in
    let cont = lower_expr st cont src2 rhs in
    let cont = lower_expr st cont src1 lhs in
    cont
  | Var { var; ty } ->
    let src = var_temp st var in
    empty
    +> [ ins (Unary { dst; op = Copy (lower_ty (Option.value_exn ty)); src }) ]
    ++ cont
;;

let lower_program st (program : Ast.program) : Tir.Func.t =
  let name = program.name in
  let start_instrs = lower_block st empty program.block in
  let start_label = add_fresh_block ~name:"start" st start_instrs in
  let next_temp_id = Id_gen.next st.temp_gen in
  let next_label_id = Id_gen.next st.label_gen in
  let blocks = st.blocks in
  let func : Tir.Func.t =
    { name
    ; blocks =
        blocks |> List.map ~f:(fun b -> b.label, b) |> Entity.Ident.Map.of_alist_exn
    ; start = start_label
    ; next_temp_id
    ; next_label_id
    }
  in
  func
;;

let lower p =
  let st = create_state () in
  lower_program st p
;;
