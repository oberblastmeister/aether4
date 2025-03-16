(* the ty field should be set to None until we do typechecking which fills it in *)
open Std
module Span = Ae_span
module Spanned = Ae_spanned

type ty =
  | Int of Span.t
  | Bool of Span.t
[@@deriving sexp_of]

module Var = struct
  module T = struct
    type t =
      { name : string [@compare.ignore] [@hash.ignore] [@equal.ignore]
      ; id : int
      ; span : Span.t [@compare.ignore] [@hash.ignore] [@equal.ignore]
      }
    [@@deriving compare, hash, equal]

    let sexp_of_t v = Sexp.Atom [%string "%{v.name}@%{v.id#Int}"]
  end

  include T
  module Map = Map.Make_plain (T)
  module Set = Set.Make_plain (T)
  module Table = Hashtbl.Make_plain (T)
end

type var = Var.t [@@deriving sexp_of, compare, hash, equal]

type stmt =
  | If of
      { cond : expr
      ; body1 : stmt
      ; body2 : stmt option
      ; span : Span.t
      }
  | Block of
      { block : block
      ; span : Span.t
      }
  | While of
      { cond : expr
      ; body : stmt
      ; span : Span.t
      }
  | Effect of expr
  | Return of
      { expr : expr
      ; span : Span.t
      }
  | Declare of
      { ty : ty
      ; var : var
      ; span : Span.t
      }
  | Assign of
      { lvalue : lvalue
      ; expr : expr
      ; span : Span.t
      }
[@@deriving sexp_of]

and lvalue = var [@@deriving sexp_of]
and block = stmt list [@@deriving sexp_of]

and expr =
  | Var of
      { var : var
      ; ty : ty option
      }
  | Int_const of int64 Spanned.t
  | Bool_const of bool Spanned.t
  | Ternary of
      { cond : expr
      ; then_expr : expr
      ; else_expr : expr
      ; ty : ty option
      ; span : Span.t
      }
  | Bin of
      { lhs : expr
      ; op : bin_op
      ; rhs : expr
      ; ty : ty option
      ; span : Span.t
      }
[@@deriving sexp_of]

and bin_op =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Lt
  | Gt
  | Le
  | Ge
  | Bit_and
  | Bit_or
  | Bit_xor
  | Eq
  | Lshift
  | Rshift
[@@deriving sexp_of]

type program =
  { ty : ty
  ; name : string
  ; block : block
  ; span : Span.t
  }
[@@deriving sexp_of]

let bool_ty = Bool Span.none
let int_ty = Int Span.none

let expr_ty_exn = function
  | Ternary { ty; _ } | Var { ty; _ } | Bin { ty; _ } -> Option.value_exn ty
  | Int_const _ -> Int Span.none
  | Bool_const _ -> Bool Span.none
;;

let nop_stmt span = Block { block = []; span }
