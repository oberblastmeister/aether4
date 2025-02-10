open Std
module Lir = Ae_lir_types
module Entity = Ae_entity_std
module Id_gen = Entity.Id_gen
module Pre_x86 = Ae_pre_x86_std
module Bag = Ae_data_bag
module Table = Entity.Name.Table
module Name = Entity.Name

let empty = Bag.empty

open Bag.Syntax

type st =
  { gen : Pre_x86.Temp_entity.Id.Witness.t Id_gen.t
  ; lir_to_pre_x86 : Pre_x86.Temp.t Lir.Temp.Table.t
  }

let create_state () =
  { gen = Id_gen.create (); lir_to_pre_x86 = Entity.Name.Table.create () }
;;

let fresh_temp ?name st : Pre_x86.Temp.t = Entity.Name.fresh ?name st.gen

let get_temp st temp =
  Table.find_or_add st.lir_to_pre_x86 temp ~default:(fun () ->
    fresh_temp ~name:temp.name st)
;;

let get_operand st temp = Pre_x86.Operand.Reg (get_temp st temp)
let fresh_operand ?name st = Pre_x86.Operand.Reg (fresh_temp ?name st)

let rec lower_expr st (dst : Pre_x86.Operand.t) (expr : Lir.Expr.t)
  : Pre_x86.Instr.t Bag.t
  =
  match expr with
  | IntConst i when Option.is_some (Int32.of_int64 i) ->
    empty +> [ Pre_x86.Instr.Mov { dst; src = Imm (Int32.of_int64_exn i) } ]
  | IntConst i -> empty +> [ Pre_x86.Instr.MovAbs { dst; src = i } ]
  | Lir.Expr.Bin { lhs; op = Add; rhs } ->
    let lhs_dst = fresh_operand ~name:"lhs" st in
    let rhs_dst = fresh_operand ~name:"rhs" st in
    let lhs_i = lower_expr st lhs_dst lhs in
    let rhs_i = lower_expr st rhs_dst rhs in
    lhs_i ++ rhs_i +> [ Pre_x86.Instr.Add { dst; src1 = lhs_dst; src2 = rhs_dst } ]
  | Lir.Expr.Bin { lhs; op = Sub; rhs } ->
    let lhs_dst = fresh_operand ~name:"lhs" st in
    let rhs_dst = fresh_operand ~name:"rhs" st in
    let lhs_i = lower_expr st lhs_dst lhs in
    let rhs_i = lower_expr st rhs_dst rhs in
    lhs_i ++ rhs_i +> [ Pre_x86.Instr.Sub { dst; src1 = lhs_dst; src2 = rhs_dst } ]
  | Lir.Expr.Bin { lhs; op; rhs } -> todol [%here]
  | Lir.Expr.Temp temp ->
    let temp = get_temp st temp in
    empty +> [ Pre_x86.Instr.Mov { dst; src = Reg temp } ]
;;

let lower_instr st (instr : Lir.Instr.t) : Pre_x86.Instr.t Bag.t =
  match instr with
  | Lir.Instr.BlockParams { temps } ->
    empty +> [ Pre_x86.Instr.BlockMov { temps = List.map temps ~f:(get_temp st) } ]
  | Lir.Instr.Assign { temp; e } ->
    let dst = get_operand st temp in
    lower_expr st dst e
  | Lir.Instr.JumpCond _ | Lir.Instr.Jump _ -> todol [%here]
  | Lir.Instr.Ret e ->
    let dst = fresh_operand ~name:"ret" st in
    let i = lower_expr st dst e in
    i +> [ Pre_x86.Instr.Ret { src = dst } ]
;;

let lower_block st (block : Lir.Block.t) : Pre_x86.Block.t =
  let body = List.map block.body ~f:(lower_instr st) |> Bag.concat |> Bag.to_list in
  { body }
;;

let lower_func st (func : Lir.Func.t) : Pre_x86.Func.t =
  let name = func.name in
  let blocks = Name.Map.map func.blocks ~f:(lower_block st) in
  let start = func.start in
  let next_id = Id_gen.next st.gen in
  { name; blocks; start; next_id }
;;

let lower func =
  let st = create_state () in
  lower_func st func
;;

let check s =
  let open struct
    module C0 = Ae_c0_std
    module Tir = Ae_tir_std
  end in
  let tokens = C0.Lexer.tokenize s in
  let program = C0.Parser.parse tokens |> Result.ok |> Option.value_exn in
  let tir = C0.Lower_tree_ir.lower program in
  let lir = Tir.Lower_lir.lower tir in
  let pre_x86 = lower lir in
  print_s [%sexp (pre_x86 : Pre_x86.Func.t)];
  ()
;;

let%expect_test "simple" =
  check
    {|
      int first() {
        int first = 12 + 1234 - 12;
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
            ((BlockMov (temps ())) (Mov (dst (Reg lhs@3)) (src (Imm 12)))
             (Mov (dst (Reg rhs@4)) (src (Imm 1234)))
             (Add (dst (Reg lhs@1)) (src1 (Reg lhs@3)) (src2 (Reg rhs@4)))
             (Mov (dst (Reg rhs@2)) (src (Imm 12)))
             (Sub (dst (Reg first@0)) (src1 (Reg lhs@1)) (src2 (Reg rhs@2)))
             (Mov (dst (Reg lhs@6)) (src (Reg first@0)))
             (Mov (dst (Reg rhs@7)) (src (Imm 12)))
             (Add (dst (Reg second@5)) (src1 (Reg lhs@6)) (src2 (Reg rhs@7)))
             (Mov (dst (Reg lhs@8)) (src (Reg second@5)))
             (Mov (dst (Reg lhs@10)) (src (Reg first@0)))
             (Mov (dst (Reg rhs@11)) (src (Reg second@5)))
             (Add (dst (Reg rhs@9)) (src1 (Reg lhs@10)) (src2 (Reg rhs@11)))
             (Add (dst (Reg second@5)) (src1 (Reg lhs@8)) (src2 (Reg rhs@9)))))))))))
     (start start@0) (next_id 12))
    |}]
;;
