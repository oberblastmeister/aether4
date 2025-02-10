open Std
module Tir = Ae_tir_types
module Entity = Ae_entity_std
module Id_gen = Entity.Id_gen
module Bag = Ae_data_bag
module Lir = Ae_lir_types

type st =
  { gen : Lir.Temp_entity.Id.Witness.t Id_gen.t
  ; tir_to_lir : (Tir.Temp_entity.Id.Witness.t, Lir.Temp.t) Entity.Name.Table.t
  }

let create_state () = { gen = Id_gen.create (); tir_to_lir = Entity.Name.Table.create () }

let get_temp t (temp : Tir.Temp.t) : Lir.Temp.t =
  Entity.Name.Table.find_or_add t.tir_to_lir temp ~default:(fun () ->
    let id = Id_gen.next t.gen in
    let temp = Entity.Name.create temp.name id in
    temp)
;;

let fresh_temp ?(name = "fresh") t : Lir.Temp.t =
  let id = Id_gen.next t.gen in
  Entity.Name.create name id
;;

let lower_bin_op (op : Tir.Bin_op.t) : Lir.Bin_op.t =
  match op with
  | Add -> Add
  | Sub -> Sub
  | Mul -> Mul
  | Div -> Div
  | Mod -> Mod
;;

let rec lower_expr st vec (expr : Tir.Expr.t) : Lir.Expr.t =
  match expr with
  | IntConst i -> IntConst i
  | Bin { lhs; op; rhs } ->
    let lhs = lower_expr st vec lhs in
    let rhs = lower_expr st vec rhs in
    let op = lower_bin_op op in
    Bin { lhs; op; rhs }
  | Temp temp ->
    let temp = get_temp st temp in
    Temp temp
;;

let lower_instr st (vec : Lir.Instr.t Vec.t) (instr : Tir.Instr.t) =
  match instr with
  | BlockParams { temps } ->
    Vec.append_list vec [ BlockParams { temps = List.map temps ~f:(get_temp st) } ]
  | Assign { temp; e } ->
    let temp = get_temp st temp in
    let e = lower_expr st vec e in
    Vec.append_list vec [ Assign { temp; e } ]
  | If _ -> todo ()
  | Jump _ -> todo ()
  | Ret e ->
    let e = lower_expr st vec e in
    Vec.append_list vec [ Ret e ]
;;

let lower_block st (block : Tir.Block.t) : Lir.Block.t =
  let vec = Vec.create () in
  List.iter block.body ~f:(lower_instr st vec);
  let body = Vec.to_list vec in
  { body }
;;

let lower_func st (func : Tir.Func.t) : Lir.Func.t =
  let name = func.name in
  let blocks = func.blocks |> Entity.Name.Map.map ~f:(lower_block st) in
  let start = func.start in
  let next_id = Id_gen.next st.gen in
  { name; blocks; start; next_id = Entity.Id.unchecked_coerce next_id }
;;

let lower func =
  let st = create_state () in
  lower_func st func
;;

let check s =
  let open struct
    module C0 = Ae_c0_std
  end in
  let tokens = C0.Lexer.tokenize s in
  let program = C0.Parser.parse tokens |> Result.ok |> Option.value_exn in
  let tir = C0.Lower_tree_ir.lower program in
  let lir = lower tir in
  print_s [%sexp (lir : Lir.Func.t)];
  ()
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
                (rhs (Bin (lhs (Temp first@0)) (op Add) (rhs (Temp second@1)))))))))))))))
     (start start@0) (next_id 2))
    |}]
;;
