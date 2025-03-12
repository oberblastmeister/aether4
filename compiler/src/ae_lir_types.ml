(* Only has 64 bit types *)
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
    | I64
    | I1
  [@@deriving sexp_of, equal, compare]
end

(* TODO: change this to use one mega Prim op type *)
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
    | And of Ty.t
    | Or of Ty.t
    | Xor of Ty.t
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
    | Int_const of
        { const : int64
        ; ty : Ty.t
        }
  [@@deriving sexp_of]
end

module Block_call = Ae_block_call.Make (Temp_entity)

module Instr = struct
  type t =
    | Block_params of { temps : (Temp.t * Ty.t) list }
    | Nop
    | Nullary of
        { dst : Temp.t
        ; op : Nullary_op.t
        }
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
  [@@deriving sexp_of, variants]

  let nop = Nop

  let is_nop = function
    | Nop -> true
    | _ -> false
  ;;

  let iter_uses instr ~f =
    match instr with
    | Block_params { temps = _ } | Nop -> ()
    | Bin { dst = _; op = _; src1; src2 } ->
      f src1;
      f src2;
      ()
    | Unary { dst = _; op = _; src } ->
      f src;
      ()
    | Nullary _ -> ()
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

  let iter_defs_with_ty (instr : t) ~f =
    match instr with
    | Block_params { temps } -> List.iter temps ~f
    | Nop -> ()
    | Bin { dst; op; src1 = _; src2 = _ } ->
      let ty : Ty.t =
        match op with
        | And ty | Or ty | Xor ty -> ty
        | Add | Sub | Mul | Div | Mod | Lshift | Rshift -> I64
        | Eq _ | Lt | Gt | Le | Ge -> I1
      in
      f (dst, ty)
    | Unary { dst; op; src = _ } ->
      (match op with
       | Copy ty -> f (dst, ty));
      ()
    | Nullary { dst; op = Int_const { const = _; ty } } ->
      f (dst, ty);
      ()
    | Jump _ | Cond_jump _ | Ret _ -> ()
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
    | Jump _ | Cond_jump _ | Ret _ -> true
    | _ -> false
  ;;

  let iter_defs instr ~f = iter_defs_with_ty instr |> Iter.map ~f:fst |> Iter.iter ~f
  let map_uses _ = todol [%here]
  let map_defs _ = todol [%here]
  let map_block_calls _ = todol [%here]
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

include Ir
