open Std
module Tir = Ae_tir
module Entity = Ae_entity_std
module Temp_entity = Ae_temp_entity
module Id_gen = Entity.Id_gen
module Append = Ae_data_append
module Lir = Ae_lir

type temp_witness

module Temp = struct
  type t = temp_witness Entity.Name.t
end

type st =
  { gen : temp_witness Id_gen.t
  ; tir_to_lir : (Tir.Temp_entity.Id.Witness.t, Temp.t) Entity.Name.Table.t
  }

let create_state () = { gen = Id_gen.create (); tir_to_lir = Entity.Name.Table.create () }

let get_temp t (temp : Tir.Temp.t) : Temp.t =
  Entity.Name.Table.find_or_add t.tir_to_lir temp ~default:(fun () ->
    let id = Id_gen.next t.gen in
    let temp = Entity.Name.create temp.name id in
    temp)
;;

let fresh_temp ?(name = "fresh") t : Temp.t =
  let id = Id_gen.next t.gen in
  Entity.Name.create name id
;;

let to_temp (temp : Temp.t) : Lir.Temp.t = Entity.Name.unchecked_coerce temp

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
    Temp (to_temp temp)
;;

let lower_instr st (vec : Lir.Instr.t Vec.t) (instr : Tir.Instr.t) =
  match instr with
  | Assign { temp; e } ->
    let temp = get_temp st temp in
    let e = lower_expr st vec e in
    Vec.append_list vec [ Assign { temp = to_temp temp; e } ]
  | If _ -> todo ()
  | Jump _ -> todo ()
  | Ret e ->
    let e = lower_expr st vec e in
    Vec.append_list vec [ Ret e ]
;;

let lower_block st (block : Tir.Block.t) : Lir.Block.t =
  let temps = List.map block.temps ~f:(fun temp -> get_temp st temp |> to_temp) in
  let vec = Vec.create () in
  List.iter block.body ~f:(lower_instr st vec);
  let body = Vec.to_list vec in
  { temps; body }
;;

let lower_func st (func : Tir.Func.t) : Lir.Func.t =
  let name = func.name in
  let params = List.map func.params ~f:(fun temp -> get_temp st temp |> to_temp) in
  let blocks = func.blocks |> Map.map ~f:(lower_block st) in
  let start = func.start in
  let next_id = Id_gen.next st.gen in
  { name; params; blocks; start; next_id = Entity.Id.unchecked_coerce next_id }
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
