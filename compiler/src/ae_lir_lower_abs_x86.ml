open Std
module Lir = Ae_lir_types
module Entity = Ae_entity_std
module Id_gen = Entity.Id_gen
module Abs_x86 = Ae_abs_x86_std
module Bag = Ae_data_bag
module Table = Entity.Name.Table
module Name = Entity.Name

let empty = Bag.empty

open Bag.Syntax

type st =
  { gen : Abs_x86.Vreg_entity.Id.Witness.t Id_gen.t
  ; lir_to_abs_x86 : Abs_x86.Vreg.t Lir.Temp.Table.t
  }

let create_state () =
  { gen = Id_gen.create (); lir_to_abs_x86 = Entity.Name.Table.create () }
;;

let fresh_temp ?name st : Abs_x86.Vreg.t = Entity.Name.fresh ?name st.gen

let get_vreg st temp =
  Table.find_or_add st.lir_to_abs_x86 temp ~default:(fun () ->
    fresh_temp ~name:temp.name st)
;;

let get_operand st temp = Abs_x86.Operand.Reg (get_vreg st temp)
let fresh_operand ?name st = Abs_x86.Operand.Reg (fresh_temp ?name st)

let rec lower_expr st (dst : Abs_x86.Operand.t) (expr : Lir.Expr.t)
  : Abs_x86.Instr.t Bag.t
  =
  match expr with
  | IntConst i when Option.is_some (Int32.of_int64 i) ->
    empty +> [ Abs_x86.Instr.Mov { dst; src = Imm (Int32.of_int64_exn i) } ]
  | IntConst i -> empty +> [ Abs_x86.Instr.MovAbs { dst; src = i } ]
  | Lir.Expr.Bin { lhs; op; rhs } ->
    let lhs_dst = fresh_operand ~name:"lhs" st in
    let rhs_dst = fresh_operand ~name:"rhs" st in
    let op : Abs_x86.Bin_op.t =
      match op with
      | Add -> Add
      | Sub -> Sub
      | Mul -> Imul
      | Div -> Idiv
      | Mod -> Imod
    in
    let lhs_i = lower_expr st lhs_dst lhs in
    let rhs_i = lower_expr st rhs_dst rhs in
    lhs_i ++ rhs_i +> [ Abs_x86.Instr.Bin { dst; src1 = lhs_dst; op; src2 = rhs_dst } ]
  | Lir.Expr.Temp temp ->
    let temp = get_vreg st temp in
    empty +> [ Abs_x86.Instr.Mov { dst; src = Reg temp } ]
;;

let lower_instr st (instr : Lir.Instr.t) : Abs_x86.Instr.t Bag.t =
  match instr with
  | Lir.Instr.BlockParams { temps } ->
    empty +> [ Abs_x86.Instr.BlockMov { temps = List.map temps ~f:(get_vreg st) } ]
  | Lir.Instr.Assign { temp; e } ->
    let dst = get_operand st temp in
    lower_expr st dst e
  | Lir.Instr.Ret e ->
    let dst = fresh_operand ~name:"ret" st in
    let i = lower_expr st dst e in
    i +> [ Abs_x86.Instr.Ret { src = dst } ]
;;

let lower_block st (block : Lir.Block.t) : Abs_x86.Block.t =
  let body = List.map block.body ~f:(lower_instr st) |> Bag.concat |> Bag.to_list in
  { body }
;;

let lower_func st (func : Lir.Func.t) : Abs_x86.Func.t =
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
  let abs_x86 = lower lir in
  print_s [%sexp (abs_x86 : Abs_x86.Func.t)];
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
             (Bin (dst (Reg lhs@1)) (op Add) (src1 (Reg lhs@3))
              (src2 (Reg rhs@4)))
             (Mov (dst (Reg rhs@2)) (src (Imm 12)))
             (Bin (dst (Reg first@0)) (op Sub) (src1 (Reg lhs@1))
              (src2 (Reg rhs@2)))
             (Mov (dst (Reg lhs@6)) (src (Reg first@0)))
             (Mov (dst (Reg rhs@7)) (src (Imm 12)))
             (Bin (dst (Reg second@5)) (op Add) (src1 (Reg lhs@6))
              (src2 (Reg rhs@7)))
             (Mov (dst (Reg lhs@8)) (src (Reg second@5)))
             (Mov (dst (Reg lhs@10)) (src (Reg first@0)))
             (Mov (dst (Reg rhs@11)) (src (Reg second@5)))
             (Bin (dst (Reg rhs@9)) (op Add) (src1 (Reg lhs@10))
              (src2 (Reg rhs@11)))
             (Bin (dst (Reg second@5)) (op Add) (src1 (Reg lhs@8))
              (src2 (Reg rhs@9)))
             (Mov (dst (Reg ret@12)) (src (Imm 0))) (Ret (src (Reg ret@12)))))))))))
     (start start@0) (next_id 13))
    |}]
;;
