open Std

type ty =
  | Int
  | Bool
[@@deriving sexp_of]

type var = string [@@deriving sexp_of]

type block = { stmts : stmt list } [@@deriving sexp_of]

and stmt =
  | Decl of decl
  | Block of block
  | Assign of assign
  | Return of expr
  | If of
      { cond : expr
      ; body1 : stmt
      ; body2 : stmt option
      }
  | While of
      { cond : expr
      ; body : stmt
      }
  | For of
      { paren : for_paren
      ; body : stmt
      }
[@@deriving sexp_of]

and for_paren =
  { init : stmt
  ; cond : expr
  ; incr : stmt
  }
[@@deriving sexp_of]

and assign =
  { lvalue : lvalue
  ; op : assign_op
  ; expr : expr
  }
[@@deriving sexp_of]

and lvalue = string [@@deriving sexp_of]

and assign_op =
  | Eq
  | Add_eq
  | Sub_eq
  | Mul_eq
  | Div_eq
  | Mod_eq
[@@deriving sexp_of]

and expr =
  | Int_const of Z.t
  | Bool_const of bool
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

type program =
  { ty : ty
  ; name : string
  ; block : block
  }
[@@deriving sexp_of]
