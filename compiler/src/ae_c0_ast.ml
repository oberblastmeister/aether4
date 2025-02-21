open Std

type ty = Int [@@deriving sexp_of]

module Var = struct
  module T = struct
    type t =
      { name : string [@compare.ignore] [@hash.ignore] [@equal.ignore]
      ; id : int
      }
    [@@deriving compare, hash, equal]

    let sexp_of_t v = Sexp.Atom [%string "%{v.name}@%{v.id#Int}"]
  end

  include T
  module Map = Map.Make_plain (T)
  module Set = Set.Make_plain (T)
  module Table = Hashtbl.Make_plain (T)
end

type var = Var.t [@@deriving sexp_of]

type stmt =
  | If of
      { cond : expr
      ; body1 : stmt
      ; body2 : stmt option
      }
  | Block of block
  | While of
      { cond : expr
      ; body : stmt
      }
  | Return of expr
  | Declare of
      { ty : ty
      ; var : var
      }
  | Assign of assign
[@@deriving sexp_of]

and assign =
  { lvalue : lvalue
  ; expr : expr
  }
[@@deriving sexp_of]

and lvalue = var [@@deriving sexp_of]
and block = stmt list [@@deriving sexp_of]

and expr =
  | Var of var
  | IntConst of int64
  | Bin of
      { lhs : expr
      ; op : bin_op
      ; rhs : expr
      }
[@@deriving sexp_of]

and bin_op =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
[@@deriving sexp_of]

type program =
  { ty : ty
  ; name : string
  ; block : block
  }
[@@deriving sexp_of]
