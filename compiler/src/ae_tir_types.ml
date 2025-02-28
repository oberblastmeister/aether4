(*
   TODO, remove the next_temp_id field and track that inside of the Table.
  The table should be able to give out next_temp_id as needed.
*)
open Std

open struct
  module Entity = Ae_entity_std
  module Generic_ir = Ae_generic_ir_std
end

module Temp_entity = Entity.Make ()
module Temp = Temp_entity.Ident
module Label_entity = Ae_label_entity
module Label = Label_entity.Ident

module Bin_op = struct
  type t =
    | Add
    | Sub
    | Mul
    | Div
    | Mod
  [@@deriving sexp_of]
end

module Ty = struct
  type t =
    | Int
    | Bool
  [@@deriving sexp_of]
end

module Unary_op = struct
  type t = Copy of Ty.t [@@deriving sexp_of]
end

module Nullary_op = struct
  type t =
    | IntConst of int64
    | BoolConst of bool
  [@@deriving sexp_of]
end

module Block_call = Ae_block_call.Make (Temp_entity)

module Instr = struct
  type t =
    | BlockParams of { temps : (Temp.t * Ty.t) list }
    | Nop
    | Bin of
        { dst : Temp.t
        ; op : Bin_op.t
        ; src1 : Temp.t
        ; src2 : Temp.t
        }
    | Unary of
        { dst : Temp.t
        ; op : Unary_op.t
        ; src : Temp.t
        }
    | Nullary of
        { dst : Temp.t
        ; op : Nullary_op.t
        }
    | Jump of Block_call.t
    | CondJump of
        { cond : Temp.t
        ; b1 : Block_call.t
        ; b2 : Block_call.t
        }
    | Ret of
        { src : Temp.t
        ; ty : Ty.t
        }
  [@@deriving sexp_of]

  let nop = Nop

  let is_nop = function
    | Nop -> true
    | _ -> false
  ;;

  let iter_uses (instr : t) ~f =
    match instr with
    | BlockParams { temps = _ } | Nop -> ()
    | Bin { dst = _; op = _; src1; src2 } ->
      f src1;
      f src2;
      ()
    | Unary { dst = _; op = _; src } ->
      f src;
      ()
    | Nullary { dst = _; op = _ } -> ()
    | Jump b ->
      Block_call.iter_uses b ~f;
      ()
    | CondJump { cond; b1; b2 } ->
      f cond;
      Block_call.iter_uses b1 ~f;
      Block_call.iter_uses b2 ~f;
      ()
    | Ret { src; ty = _ } ->
      f src;
      ()
  ;;

  let iter_defs (instr : t) ~f =
    match instr with
    | BlockParams { temps } -> List.iter temps ~f:(fun (temp, _) -> f temp)
    | Nop -> ()
    | Bin { dst; op = _; src1 = _; src2 = _ } ->
      f dst;
      ()
    | Unary { dst; op = _; src = _ } ->
      f dst;
      ()
    | Nullary { dst; op = _ } ->
      f dst;
      ()
    | Jump _ | CondJump _ | Ret _ -> ()
  ;;

  let jumps (instr : t) =
    match instr with
    | Nop | BlockParams _ | Bin _ | Unary _ | Nullary _ -> None
    | Ret _ -> Some []
    | Jump b -> Some [ b ]
    | CondJump { cond = _; b1; b2 } -> Some [ b1; b2 ]
  ;;

  let map_block_calls (instr : t) ~f =
    match instr with
    | Jump b -> Jump (f b)
    | CondJump { cond; b1; b2 } -> CondJump { cond; b1 = f b1; b2 = f b2 }
    | _ -> instr
  ;;

  let map_uses (instr : t) ~f =
    match instr with
    | Nop -> Nop
    | BlockParams _ -> instr
    | Bin p ->
      let src1 = f p.src1 in
      let src2 = f p.src2 in
      Bin { p with src1; src2 }
    | Unary p ->
      let src = f p.src in
      Unary { p with src }
    | Nullary _ -> instr
    | Jump j ->
      let j = Block_call.map_uses j ~f in
      Jump j
    | CondJump { cond; b1; b2 } ->
      let cond = f cond in
      let b1 = Block_call.map_uses b1 ~f in
      let b2 = Block_call.map_uses b2 ~f in
      CondJump { cond; b1; b2 }
    | Ret p ->
      let src = f p.src in
      Ret { p with src }
  ;;

  let map_defs (instr : t) ~f =
    match instr with
    | BlockParams p ->
      BlockParams { p with temps = (List.map & Tuple2.map_fst) p.temps ~f }
    | Nop -> Nop
    | Bin p -> Bin { p with dst = f p.dst }
    | Unary p -> Unary { p with dst = f p.dst }
    | Nullary p -> Nullary { p with dst = f p.dst }
    | Jump _ -> instr
    | CondJump _ -> instr
    | Ret _ -> instr
  ;;
end

module Ir = Generic_ir.Make_ir (struct
    module Instr = Instr

    module Func_data = struct
      type t = unit [@@deriving sexp_of]
    end

    module Temp_entity = Temp_entity
  end)

include Ir
module Liveness = Generic_ir.Liveness.Make (Ir)
