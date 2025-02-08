open Std
module Cst = Ae_c0_cst
module Tir = Ae_tir_types
module Entity = Ae_entity_std
module Temp = Tir.Temp
module Id_gen = Entity.Id_gen
module Bag = Ae_data_bag
open Bag.Syntax

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

let rec lower_program st (program : Cst.program) : Tir.Func.t =
  let name = program.name in
  let params = [] in
  let start_label = Id_gen.next st.label_gen |> Entity.Name.create "start" in
  let instrs = lower_block st program.block |> Bag.to_list in
  let start_block = { Tir.Block.temps = []; body = instrs } in
  let func : Tir.Func.t =
    { name
    ; params
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
  | Ident var ->
    let temp = var_temp st var in
    let expr_i, expr = lower_expr st assign.expr in
    let bin_expr =
      match assign.op with
      | Eq -> expr
      | AddEq -> Tir.Expr.Bin { lhs = Temp temp; op = Add; rhs = expr }
      | _ -> todo ()
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

and lower_expr st (expr : Cst.expr) : _ * Tir.Expr.t =
  match expr with
  | IntConst i -> Bag.of_list [], Tir.Expr.IntConst i
  | Bin { lhs; op; rhs } ->
    let op = lower_bin_op op in
    let lhs_instr, lhs = lower_expr st lhs in
    let rhs_instr, rhs = lower_expr st rhs in
    lhs_instr ++ rhs_instr, Bin { lhs; op; rhs }
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
    ((name bruh) (params ())
     (blocks ((((name start) (id 0)) ((temps ()) (body ())))))
     (start ((name start) (id 0))) (next_id 0))
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
    ((name first) (params ())
     (blocks
      ((((name start) (id 0))
        ((temps ())
         (body
          ((Assign (temp ((name first) (id 0)))
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
           (Assign (temp ((name second) (id 1)))
            (e
             (Bin (lhs (Temp ((name first) (id 0)))) (op Add)
              (rhs (IntConst 12)))))
           (Assign (temp ((name second) (id 1)))
            (e
             (Bin (lhs (Temp ((name second) (id 1)))) (op Add)
              (rhs
               (Bin (lhs (Temp ((name first) (id 0)))) (op Add)
                (rhs (Temp ((name second) (id 1)))))))))))))))
     (start ((name start) (id 0))) (next_id 2))
    |}]
;;
