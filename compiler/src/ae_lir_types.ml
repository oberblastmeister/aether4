(* Only has 64 bit types *)
open Std

open struct
  module Entity = Ae_entity_std
end

module Temp_entity = Entity.Make ()
module Temp = Temp_entity.Ident
module Label_entity = Ae_label_entity
module Label = Label_entity.Ident
module Generic_ir = Ae_generic_ir_std

(* TODO: change this to use one mega Prim op type *)
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
    | I64
    | I1
  [@@deriving sexp_of]
end

module Unary_op = struct
  type t = Copy of Ty.t [@@deriving sexp_of]
end

module Block_call = struct
  type t =
    { label : Label.t
    ; args : Temp.t list
    }
  [@@deriving sexp_of]
end

module Instr = struct
  type t =
    | BlockParams of { temps : (Temp.t * Ty.t) list }
    | Nop
    | IntConst of
        { dst : Temp.t
        ; const : int64
        ; ty : Ty.t
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

  let iter_uses _ = todol [%here]
  let iter_defs _ = todol [%here]
  let jumps _ = todol [%here]
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
