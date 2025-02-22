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
  type t =
    | Int
    | Bool
  [@@deriving sexp_of]
end

module Unary_op = struct
  type t = Copy of Ty.t [@@deriving sexp_of]
end

module Instr = struct
  type t =
    | BlockParams of { temps : (Temp.t * Ty.t) list }
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
    | IntConst of
        { dst : Temp.t
        ; const : int64
        }
    | Ret of
        { src : Temp.t
        ; ty : Ty.t
        }
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
  we want a transaction data structures
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
