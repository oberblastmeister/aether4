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

module Block_param = struct
  type t =
    { param : Temp.t
    ; ty : Ty.t
    }
  [@@deriving sexp_of, fields]

  let to_tuple2 { param; ty } = param, ty
end

module Block_call = struct
  type t =
    { label : Label.t
    ; args : Temp.t list
    }
  [@@deriving sexp_of, fields ~fields ~getters ~iterators:create]
end

module Instr = struct
  type t =
    | Block_params of Block_param.t list
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
    | Call of
        { dst : Temp.t
        ; ty : Ty.t
        ; args : (Temp.t * Ty.t) list
        }
    | Unreachable
  [@@deriving sexp_of, variants]

  let empty_block_params = Block_params []

  let block_params_tys = function
    | Block_params params -> Some (List.map ~f:Block_param.ty params)
    | _ -> None
  ;;

  let nop = Nop

  let is_nop = function
    | Nop -> true
    | _ -> false
  ;;

  let iter_uses (instr : t) ~f =
    match instr with
    | Unreachable | Block_params _ | Nop -> ()
    | Call { dst = _; ty = _; args } -> (List.iter @> Fold.of_fn fst) args ~f
    | Bin { dst = _; op = _; src1; src2 } ->
      f src1;
      f src2;
      ()
    | Unary { dst = _; op = _; src } ->
      f src;
      ()
    | Nullary { dst = _; op = _ } -> ()
    | Jump b ->
      List.iter b.args ~f;
      ()
    | Cond_jump { cond; b1; b2 } ->
      f cond;
      List.iter b1.args ~f;
      List.iter b2.args ~f;
      ()
    | Ret { src; ty = _ } ->
      f src;
      ()
  ;;

  let iter_uses_with_known_ty (instr : t) ~f =
    match instr with
    | Unreachable | Block_params _ | Nop -> ()
    | Call _ -> ()
    | Bin { dst = _; op; src1; src2 } ->
      let ty : Ty.t =
        match op with
        | Add
        | Sub
        | Mul
        | Div
        | Mod
        | And
        | Or
        | Xor
        | Lshift
        | Rshift
        | Lt
        | Gt
        | Le
        | Ge -> Int
        | Eq ty -> ty
      in
      f (src1, ty);
      f (src2, ty);
      ()
    | Unary { dst = _; op; src } ->
      (match op with
       | Copy ty -> f (src, ty));
      ()
    | Nullary { dst = _; op = _ } -> ()
    | Jump _b -> ()
    | Cond_jump { cond; b1 = _; b2 = _ } ->
      f (cond, Bool);
      ()
    | Ret { src; ty } ->
      f (src, ty);
      ()
  ;;

  let iter_defs_with_ty t ~f =
    match t with
    | Block_params params -> (List.iter @> Fold.of_fn Block_param.to_tuple2) params ~f
    | Nop -> ()
    | Call { dst; ty; args = _ } ->
      f (dst, ty);
      ()
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
    | Call p ->
      let args = (List.map & Tuple2.map_fst) p.args ~f in
      Call { p with args }
    | Bin p ->
      let src1 = f p.src1 in
      let src2 = f p.src2 in
      Bin { p with src1; src2 }
    | Unary p ->
      let src = f p.src in
      Unary { p with src }
    | Nullary _ -> instr
    | Jump j ->
      let j = (Traverse.of_field Block_call.Fields.args & List.map) j ~f in
      Jump j
    | Cond_jump { cond; b1; b2 } ->
      let cond = f cond in
      let b1 = (Traverse.of_field Block_call.Fields.args & List.map) b1 ~f in
      let b2 = (Traverse.of_field Block_call.Fields.args & List.map) b2 ~f in
      Cond_jump { cond; b1; b2 }
    | Ret p ->
      let src = f p.src in
      Ret { p with src }
  ;;

  let map_defs (instr : t) ~f =
    match instr with
    | Unreachable -> Unreachable
    | Block_params params ->
      Block_params ((List.map & Traverse.of_field Block_param.Fields.param) params ~f)
    | Nop -> Nop
    | Bin p -> Bin { p with dst = f p.dst }
    | Unary p -> Unary { p with dst = f p.dst }
    | Nullary p -> Nullary { p with dst = f p.dst }
    | Jump _ -> instr
    | Cond_jump _ -> instr
    | Ret _ -> instr
    | Call p -> Call { p with dst = f p.dst }
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

module Location = struct
  include Temp

  let temp_val t = Some t
  let of_temp t = t
end

module Ann = struct
  type t = unit [@@deriving sexp_of]

  let default = ()
end
