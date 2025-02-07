open Std

open struct
  module Entity = Ae_entity_std
end

module Temp_entity = Entity.Make ()
module Temp = Temp_entity.Name
module Label_entity = Ae_label_entity
module Label = Label_entity.Name

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
  type t = Int [@@deriving sexp_of]
end

module Expr = struct
  type t =
    | IntConst of int
    | Bin of
        { lhs : t
        ; op : Bin_op.t
        ; rhs : t
        }
    | Temp of Temp.t
  [@@deriving sexp_of]
end

module Instr = struct
  type t =
    | Assign of
        { temp : Temp.t
        ; e : Expr.t
        }
    | JumpCond of
        { cond : Expr.t
        ; l1 : Label.t
        ; l2 : Label.t
        }
    | Jump of
        { l : Label.t
        ; args : Temp.t list
        }
    | Ret of Expr.t
  [@@deriving sexp_of]
end

module Block = struct
  type t =
    { temps : Temp.t list
    ; body : Instr.t list
    }
  [@@deriving sexp_of]
end

module Func = struct
  type t =
    { name : string
    ; params : Temp.t list
    ; blocks : Block.t Label.Map.t
    ; start : Label.t
    ; next_id : Temp_entity.Id.t
    }
  [@@deriving sexp_of]
end
