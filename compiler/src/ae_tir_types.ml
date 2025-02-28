(*
   TODO, remove the next_temp_id field and track that inside of the Table.
  The table should be able to give out next_temp_id as needed.
*)
open Std

open struct
  module Entity = Ae_entity_std
end

module Temp_entity = Entity.Make ()
module Temp = Temp_entity.Ident
module Label_entity = Ae_label_entity
module Label = Label_entity.Ident
module Generic_ir = Ae_generic_ir_std

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

module Block_call = struct
  type t =
    { label : Label.t
    ; args : Temp.t list
    }
  [@@deriving sexp_of]

  let create ?(args = []) label = { label; args }
  let iter_uses t ~f = List.iter t.args ~f
end

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
    | Jump b -> Some [ b.label ]
    | CondJump { cond = _; b1; b2 } -> Some [ b1.label; b2.label ]
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
