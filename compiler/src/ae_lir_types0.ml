(* Only has 64 bit types *)
open Std

open struct
  module Generic_ir = Ae_generic_ir_std
  module Call_conv = Ae_call_conv
end

module Temp = Ae_temp
module Label = Ae_label

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

module Block_call = struct
  type t =
    { label : Label.t
    ; args : Temp.t list
    }
  [@@deriving sexp_of, fields ~fields ~getters ~iterators:create]
end

module Block_param = struct
  type t =
    { param : Temp.t
    ; ty : Ty.t
    }
  [@@deriving sexp_of, fields]
end

module Instr = struct
  type t =
    | Block_params of Block_param.t list
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
    | Call of
        { dst : Temp.t
        ; ty : Ty.t
        ; func : string
        ; args : (Temp.t * Ty.t) list
        ; call_conv : Call_conv.t
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

  let iter_uses instr ~f =
    match instr with
    | Block_params _ | Nop -> ()
    | Call { dst = _; func = _; ty = _; args } -> (List.iter @> Fold.of_fn fst) args ~f
    | Bin { dst = _; op = _; src1; src2 } ->
      f src1;
      f src2;
      ()
    | Unary { dst = _; op = _; src } ->
      f src;
      ()
    | Nullary _ -> ()
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
    | Unreachable -> ()
  ;;

  let iter_uses_with_known_ty (instr : t) ~f =
    match instr with
    | Unreachable | Block_params _ | Nop -> ()
    | Call { args; _ } ->
      List.iter args ~f;
      ()
    | Bin { dst = _; op; src1; src2 } ->
      let ty : Ty.t =
        match op with
        | Add | Sub | Mul | Div | Mod | Lshift | Rshift | Lt | Gt | Le | Ge -> I64
        | And ty | Or ty | Xor ty | Eq ty -> ty
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
      f (cond, I1);
      ()
    | Ret { src; ty } ->
      f (src, ty);
      ()
  ;;

  let iter_defs_with_ty (instr : t) ~f =
    match instr with
    | Block_params params ->
      List.iter params ~f:(fun param -> f (Block_param.param param, Block_param.ty param))
    | Nop -> ()
    | Call { dst; func = _; ty; args = _ } ->
      f (dst, ty);
      ()
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
    | Unreachable | Jump _ | Cond_jump _ | Ret _ -> ()
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

  let iter_defs instr ~f = iter_defs_with_ty instr |> Iter.map ~f:fst |> Iter.iter ~f
  let map_uses _ = todol [%here]
  let map_defs _ = todol [%here]
  let map_block_calls _ = todol [%here]
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
