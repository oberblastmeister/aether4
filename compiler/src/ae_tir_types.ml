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

module Ty = struct
  type t =
    | Int
    | Bool
  [@@deriving sexp_of, equal, compare]
end

module Bin_op = struct
  type t =
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    | Lt
    | Gt
    | Le
    | Ge
    | And
    | Or
    | Xor
    | Eq of Ty.t
    | Lshift
    | Rshift
  [@@deriving sexp_of]
end

module Unary_op = struct
  type t = Copy of Ty.t [@@deriving sexp_of]
end

module Nullary_op = struct
  type t =
    | Int_const of int64
    | Bool_const of bool
  [@@deriving sexp_of]
end

module Block_call = Ae_block_call.Make (Temp_entity)

module Instr = struct
  type t =
    | Block_params of { temps : (Temp.t * Ty.t) list }
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
    | Cond_jump of
        { cond : Temp.t
        ; b1 : Block_call.t
        ; b2 : Block_call.t
        }
    | Ret of
        { src : Temp.t
        ; ty : Ty.t
        }
    | Unreachable
  [@@deriving sexp_of, variants]

  let nop = Nop

  let is_nop = function
    | Nop -> true
    | _ -> false
  ;;

  let iter_uses (instr : t) ~f =
    match instr with
    | Unreachable | Block_params { temps = _ } | Nop -> ()
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
    | Cond_jump { cond; b1; b2 } ->
      f cond;
      Block_call.iter_uses b1 ~f;
      Block_call.iter_uses b2 ~f;
      ()
    | Ret { src; ty = _ } ->
      f src;
      ()
  ;;

  let iter_defs_with_ty t ~f =
    match t with
    | Block_params { temps } -> List.iter temps ~f
    | Nop -> ()
    | Bin { dst; op; src1 = _; src2 = _ } ->
      let ty : Ty.t =
        match op with
        | Add | Sub | Mul | Div | Mod | And | Or | Xor | Lshift | Rshift -> Int
        | Eq _ | Lt | Gt | Le | Ge -> Bool
      in
      f (dst, ty)
    | Unary { dst; op; src = _ } ->
      (match op with
       | Copy ty -> f (dst, ty));
      ()
    | Nullary { dst; op } ->
      (match op with
       | Int_const _ -> f (dst, Int)
       | Bool_const _ -> f (dst, Bool));
      ()
    | Unreachable | Jump _ | Cond_jump _ | Ret _ -> ()
  ;;

  let iter_defs t ~f = iter_defs_with_ty t |> Iter.map ~f:fst |> Iter.iter ~f

  let map_block_calls (instr : t) ~f =
    match instr with
    | Jump b -> Jump (f b)
    | Cond_jump { cond; b1; b2 } -> Cond_jump { cond; b1 = f b1; b2 = f b2 }
    | _ -> instr
  ;;

  let map_uses (instr : t) ~f =
    match instr with
    | Unreachable -> Unreachable
    | Nop -> Nop
    | Block_params _ -> instr
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
    | Cond_jump { cond; b1; b2 } ->
      let cond = f cond in
      let b1 = Block_call.map_uses b1 ~f in
      let b2 = Block_call.map_uses b2 ~f in
      Cond_jump { cond; b1; b2 }
    | Ret p ->
      let src = f p.src in
      Ret { p with src }
  ;;

  let map_defs (instr : t) ~f =
    match instr with
    | Unreachable -> Unreachable
    | Block_params p ->
      Block_params { p with temps = (List.map & Tuple2.map_fst) p.temps ~f }
    | Nop -> Nop
    | Bin p -> Bin { p with dst = f p.dst }
    | Unary p -> Unary { p with dst = f p.dst }
    | Nullary p -> Nullary { p with dst = f p.dst }
    | Jump _ -> instr
    | Cond_jump _ -> instr
    | Ret _ -> instr
  ;;

  let iter_block_calls t ~f =
    match t with
    | Ret _ -> ()
    | Jump b ->
      f b;
      ()
    | Cond_jump { cond = _; b1; b2 } ->
      f b1;
      f b2;
      ()
    | _ -> ()
  ;;

  let is_control = function
    | Unreachable | Jump _ | Cond_jump _ | Ret _ -> true
    | _ -> false
  ;;

  let move ~dst ~src ~ty = Unary { dst; op = Copy ty; src }
end

include Generic_ir.Make_all (struct
    module Block_call = Block_call
    module Instr = Instr
    module Ty = Ty

    module Func_data = struct
      type t = unit [@@deriving sexp_of]
    end

    module Temp_entity = Temp_entity
  end)

module Destruct_ssa = Generic_ir.Destruct_ssa.Make (Ir) (Instr)
include Ir
