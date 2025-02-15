open Std
module Cst = Ae_c0_cst
module Tir = Ae_tir_types
module Entity = Ae_entity_std
module Temp = Tir.Temp
module Id_gen = Entity.Id_gen
module Bag = Ae_data_bag
open Bag.Syntax

let empty = Bag.empty

module Lower = Ae_monad_bag_writer.Make (struct
    type t = Tir.Instr.t
  end)

open Lower.Syntax

type st =
  { gen : Tir.Temp_entity.Id.Witness.t Id_gen.t
  ; var_to_temp : Temp.t String.Table.t
  ; label_gen : Tir.Label_entity.Id.Witness.t Id_gen.t
  }

let create_state () =
  { gen = Id_gen.create ()
  ; var_to_temp = String.Table.create ()
  ; label_gen = Id_gen.create ()
  }
;;

let var_temp t var =
  Hashtbl.find_or_add t.var_to_temp var ~default:(fun () ->
    let id = Id_gen.next t.gen in
    let temp = { Entity.Name.id; name = var } in
    temp)
;;

let fresh_temp t : Temp.t =
  let id = Id_gen.next t.gen in
  Entity.Name.create "fresh" id
;;

let assign_op_to_op_exn (op_assign : Cst.assign_op) : Cst.bin_op =
  match op_assign with
  | Eq -> assert false
  | AddEq -> Add
  | SubEq -> Sub
  | MulEq -> Mul
  | DivEq -> Div
  | ModEq -> Mod
;;

let rec lower_program st (program : Cst.program) : Tir.Func.t =
  let name = program.name in
  let start_label = Id_gen.next st.label_gen |> Entity.Name.create "start" in
  let instrs =
    empty
    +> [ Tir.Instr.BlockParams { temps = [] } ]
    ++ lower_block st program.block
    +> [ Tir.Instr.Ret (IntConst 0L) ]
  in
  let start_block = { Tir.Block.body = Bag.to_list instrs } in
  let func : Tir.Func.t =
    { name
    ; blocks = Entity.Name.Map.singleton start_label start_block
    ; start = start_label
    ; next_id = Id_gen.next st.gen
    }
  in
  func

and lower_block st (block : Cst.block) =
  block.stmts |> List.map ~f:(lower_stmt st) |> Bag.concat

and lower_stmt st (stmt : Cst.stmt) =
  match stmt with
  | Decl decl -> lower_decl st decl
  | Block block -> lower_block st block
  | Assign assign -> lower_assign st assign
  | Return expr -> lower_return st expr

and lower_assign st (assign : Cst.assign) : Tir.Instr.t Bag.t =
  match assign.lvalue with
  | var ->
    let temp = var_temp st var in
    let expr_i, expr = lower_expr st assign.expr in
    let bin_expr =
      match assign.op with
      | Eq -> expr
      | assign_op ->
        let op = assign_op_to_op_exn assign_op |> lower_bin_op in
        Tir.Expr.Bin { lhs = Temp temp; op; rhs = expr }
    in
    expr_i +> [ Tir.Instr.Assign { temp; e = bin_expr } ]

and lower_return st expr =
  let expr_i, expr = lower_expr st expr in
  expr_i +> [ Tir.Instr.Ret expr ]

and lower_bin_op (op : Cst.bin_op) : Tir.Bin_op.t =
  match op with
  | Add -> Add
  | Sub -> Sub
  | Mul -> Mul
  | Div -> Div
  | Mod -> Mod

and lower_expr st (expr : Cst.expr) : Tir.Expr.t Lower.t =
  match expr with
  | IntConst i -> Bag.of_list [], Tir.Expr.IntConst (Z.to_int64_exn i)
  | Bin { lhs; op; rhs } ->
    let op = lower_bin_op op in
    let+ lhs = lower_expr st lhs
    and+ rhs = lower_expr st rhs in
    Tir.Expr.Bin { lhs; op; rhs }
  | Cst.Neg _ -> todo ()
  | Cst.Var v ->
    let temp = var_temp st v in
    Bag.empty, Temp temp

and lower_decl st (decl : Cst.decl) =
  let temp = var_temp st decl.name in
  match decl.expr with
  | None -> Bag.empty
  | Some expr ->
    let expr_instr, expr = lower_expr st expr in
    expr_instr +> [ Tir.Instr.Assign { temp; e = expr } ]
;;

let lower p =
  let st = create_state () in
  lower_program st p
;;

let check s =
  let open struct
    module Lexer = Ae_c0_lexer
    module Parser = Ae_c0_parser
  end in
  let tokens = Lexer.tokenize s in
  let program = Parser.parse tokens |> Result.ok |> Option.value_exn in
  let tir = lower program in
  print_s [%sexp (tir : Tir.Func.t)];
  ()
;;

let%expect_test "simple" =
  check
    {|
    int bruh() {
    
    }
  |};
  [%expect
    {|
    ((name bruh)
     (blocks
      ((0
        ((key start@0)
         (data ((body ((BlockParams (temps ())) (Ret (IntConst 0))))))))))
     (start start@0) (next_id 0))
    |}]
;;

let%expect_test "simple decl" =
  check
    {|
    int first() {
      int first = 12 + 1234 % 1234 * 12 / 2;
      int second = first + 12;
      second += first + second;
    }
  |};
  [%expect
    {|
    ((name first)
     (blocks
      ((0
        ((key start@0)
         (data
          ((body
            ((BlockParams (temps ()))
             (Assign (temp first@0)
              (e
               (Bin (lhs (IntConst 12)) (op Add)
                (rhs
                 (Bin
                  (lhs
                   (Bin
                    (lhs
                     (Bin (lhs (IntConst 1234)) (op Mod) (rhs (IntConst 1234))))
                    (op Mul) (rhs (IntConst 12))))
                  (op Div) (rhs (IntConst 2)))))))
             (Assign (temp second@1)
              (e (Bin (lhs (Temp first@0)) (op Add) (rhs (IntConst 12)))))
             (Assign (temp second@1)
              (e
               (Bin (lhs (Temp second@1)) (op Add)
                (rhs (Bin (lhs (Temp first@0)) (op Add) (rhs (Temp second@1)))))))
             (Ret (IntConst 0))))))))))
     (start start@0) (next_id 2))
    |}]
;;
