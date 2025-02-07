open Std

type ty = Int [@@deriving sexp_of]

type program =
  { ty : ty
  ; name : string
  ; block : block
  }
[@@deriving sexp_of]

and block = { stmts : stmt list } [@@deriving sexp_of]

and stmt =
  | Decl of decl
  | Block of block
  | Assign of assign
  | Return of expr
[@@deriving sexp_of]

and assign =
  { lvalue : lvalue
  ; op : assign_op
  ; expr : expr
  }
[@@deriving sexp_of]

and lvalue = Ident of string [@@deriving sexp_of]

and assign_op =
  | Eq
  | AddEq
  | SubEq
  | MulEq
  | DivEq
  | ModEq
[@@deriving sexp_of]

and expr =
  | IntConst of int
  | Var of string
  | Neg of expr
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

and decl =
  { ty : ty
  ; name : string
  ; expr : expr option
  }
[@@deriving sexp_of]
