(*
   TODO, remove the next_id field and track that inside of the Table.
  The table should be able to give out next_id as needed.
*)
open Std

open struct
  module Entity = Ae_entity_std
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
  type t = Int [@@deriving sexp_of]
end

module Expr = struct
  type t =
    | IntConst of int64
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
    | BlockParams of { temps : Temp.t list }
    | Assign of
        { temp : Temp.t
        ; e : Expr.t
        }
    (* | If of
        { cond : Expr.t
        ; body1 : t list
        ; body2 : t list
        }
    | Jump of
        { l : Label.t
        ; args : Temp.t list
        } *)
    | Ret of Expr.t
  [@@deriving sexp_of]
end

module Block = struct
  type t = { body : Instr.t list } [@@deriving sexp_of]
end

module Func = struct
  type t =
    { name : string
    ; blocks : Block.t Label.Map.t
    ; start : Label.t
    ; next_id : Temp_entity.Id.t
    }
  [@@deriving sexp_of]
end

(*
   notes
  we want a transaction data structures.
*)
module Block_transaction = struct
  type t =
    | Insert of
        { index : int
        ; instr : Instr.t
        }
    | Remove of int
end

module Func_transaction = struct
  type t =
    { mutable next_temp_id : Temp_entity.Id.t
    ; mutable next_label_id : Label_entity.Id.t
    ; mutable block_operations : Block_transaction.t list Label.Map.t
    }
end
