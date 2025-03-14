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
  | Effect of expr
  | Post of
      { lvalue : lvalue
      ; op : post_op
      }
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
  { init : stmt option
  ; cond : expr
  ; incr : stmt option
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
  | Id_assign
  | Add_assign
  | Sub_assign
  | Mul_assign
  | Div_assign
  | Mod_assign
  | Bit_and_assign
  | Bit_or_assign
  | Bit_xor_assign
  | Lshift_assign
  | Rshift_assign
[@@deriving sexp_of]

and post_op =
  | Incr
  | Decr

and expr =
  | Int_const of Z.t
  | Bool_const of bool
  | Var of string
  | Unary of
      { op : unary_op
      ; expr : expr
      }
  | Bin of
      { lhs : expr
      ; op : bin_op
      ; rhs : expr
      }
  | Ternary of
      { cond : expr
      ; then_expr : expr
      ; else_expr : expr
      }
[@@deriving sexp_of]

and unary_op =
  | Neg
  | Bit_not
  | Log_not

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
  | Log_and
  | Log_or
  | Lshift
  | Rshift
  | Eq
  | Neq
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

let bin ~lhs ~op ~rhs = Bin { lhs; rhs; op }
let nop_stmt = Block { stmts = [] }
