(*
   TODO, remove the next_temp_id field and track that inside of the Table.
  The table should be able to give out next_temp_id as needed.
*)
open Std

open struct
  module Generic_ir = Ae_generic_ir_std
  module Struct_layout = Ae_struct_layout
end

module Temp = Ae_temp
module Label = Ae_label

module Ty = struct
  (* TODO: maybe add a struct type because it is still useful *)
  type t =
    | Int
    | Bool
    | Void
    | Ptr
  [@@deriving sexp_of, equal, compare]

  let size_of ty =
    match ty with
    | Int -> 8
    | Bool -> 1
    (* TODO: make this zero some time, so allocating Void will not allocate anything *)
    | Void -> 1
    | Ptr -> 8
  ;;

  let align_of ty =
    match ty with
    | Int -> 8
    | Bool -> 1
    | Void -> 1
    | Ptr -> 8
  ;;
end

(* module Struct = struct
  type t = Ty.strukt [@@deriving sexp_of, equal, compare]

  let create tys : t =
    let layout =
      List.map tys ~f:(fun ty ->
        Struct_layout.Field.{ size = Ty.size_of ty; align = Ty.align_of ty })
      |> Struct_layout.calculate
    in
    let tys = Arrayp.of_list tys in
    let fields =
      Arrayp.zip_exn tys layout.offsets
      |> Arrayp.map ~f:(fun (ty, offset) -> { Struct_field.ty; offset })
    in
    { fields; align = layout.align; size = layout.size }
  ;;
end *)

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
    | Store of Ty.t
    | Offset_ptr
  [@@deriving sexp_of]
end

module Place = struct
  type t = | [@@deriving sexp_of]

  let iter_uses (t : t) =
    match t with
    | _ -> .
  ;;

  let map_uses (t : t) =
    match t with
    | _ -> .
  ;;

  let iter_uses_with_known_ty (t : t) =
    match t with
    | _ -> .
  ;;
end

module Unary_op = struct
  type t =
    | Copy of Ty.t
    | Deref of Ty.t
    | Alloc_array of
        { size : int
        ; align : int
        }
  [@@deriving sexp_of]
end

module Nullary_op = struct
  type t =
    | Int_const of int64
    | Null_ptr
    | Bool_const of bool
    | Void_const
    | Alloc of
        { size : int
        ; align : int
        }
  [@@deriving sexp_of]
end

module Nary_op = struct
  type t =
    | C0_runtime_assert
    | C0_runtime_null_pointer_panic
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
    | Nary of
        { dst : Temp.t
        ; op : Nary_op.t
        ; srcs : Temp.t list
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
        ; func : string
        ; args : (Temp.t * Ty.t) list
        ; is_extern : bool
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
    | Call { args; _ } -> (List.iter @> Fold.of_fn fst) args ~f
    | Bin { dst = _; op = _; src1; src2 } ->
      f src1;
      f src2;
      ()
    | Unary { dst = _; op = _; src } ->
      f src;
      ()
    | Nary { dst = _; op = _; srcs } ->
      List.iter srcs ~f;
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
    | Bin { dst = _; op = Store ty; src1; src2 } ->
      f (src1, Ty.Ptr);
      f (src2, ty)
    | Bin { dst = _; op = Offset_ptr; src1; src2 } ->
      f (src1, Ptr);
      f (src2, Int);
      ()
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
        | Store _ | Offset_ptr -> assert false
      in
      f (src1, ty);
      f (src2, ty);
      ()
    | Unary { dst = _; op; src } -> begin
      match op with
      | Copy ty -> f (src, ty)
      | Deref _ty -> f (src, Ptr)
      | Alloc_array _ -> f (src, Int)
    end
    | Nullary { dst = _; op = _ } -> ()
    | Jump _b -> ()
    | Cond_jump { cond; b1 = _; b2 = _ } ->
      f (cond, Bool);
      ()
    | Nary { dst = _; op; srcs } -> begin
      match op, srcs with
      | C0_runtime_assert, [ t0; t1; t2; t3; t4 ] ->
        f (t0, Bool);
        f (t1, Int);
        f (t2, Int);
        f (t3, Int);
        f (t4, Int);
        ()
      | C0_runtime_null_pointer_panic, [] -> ()
      | (C0_runtime_assert | C0_runtime_null_pointer_panic), _ ->
        raise_s [%message "Invalid Nary operation"]
    end
    | Ret { src; ty } ->
      f (src, ty);
      ()
  ;;

  let iter_defs_with_ty t ~f =
    match t with
    | Block_params params -> (List.iter @> Fold.of_fn Block_param.to_tuple2) params ~f
    | Nary { dst; op; srcs = _ } -> begin
      match op with
      | C0_runtime_assert -> f (dst, Void)
      | C0_runtime_null_pointer_panic -> f (dst, Void)
    end
    | Nop -> ()
    | Call { dst; ty; _ } ->
      f (dst, ty);
      ()
    | Bin { dst; op; src1 = _; src2 = _ } ->
      let ty : Ty.t =
        match op with
        | Add | Sub | Mul | Div | Mod | And | Or | Xor | Lshift | Rshift -> Int
        | Eq _ | Lt | Gt | Le | Ge -> Bool
        | Store _ -> Void
        | Offset_ptr -> Ptr
      in
      f (dst, ty)
    | Unary { dst; op; src = _ } -> begin
      match op with
      | Copy ty -> f (dst, ty)
      | Deref ty -> f (dst, ty)
      | Alloc_array _ -> f (dst, Ptr)
    end
    | Nullary { dst; op } -> begin
      match op with
      | Int_const _ -> f (dst, Int)
      | Bool_const _ -> f (dst, Bool)
      | Void_const -> f (dst, Void)
      | Alloc _size -> f (dst, Ptr)
      | Null_ptr -> f (dst, Ptr)
    end
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
    | Nary p ->
      let srcs = List.map ~f p.srcs in
      Nary { p with srcs }
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
    | Nary p -> Nary { p with dst = f p.dst }
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
