open Std
module Spanned = Ae_spanned
module Span = Ae_span

type var = string Spanned.t [@@deriving sexp_of]

type ty =
  | Int of Span.t
  | Bool of Span.t
  | Ty_var of var
  | Void of Span.t
[@@deriving sexp_of]

type block =
  { block : stmt list
  ; span : Span.t
  }
[@@deriving sexp_of]

and stmt =
  | Decl of
      { ty : ty
      ; names : var list
      ; expr : expr option
      ; span : Span.t
      }
  | Block of block
  | Assign of assign
  | Effect of expr
  | Post of
      { lvalue : lvalue
      ; op : post_op
      ; span : Span.t
      }
  | Return of
      { expr : expr option
      ; span : Span.t
      }
  | Assert of
      { expr : expr
      ; span : Span.t
      }
  | If of
      { cond : expr
      ; body1 : stmt
      ; body2 : stmt option
      ; span : Span.t
      }
  | While of
      { cond : expr
      ; body : stmt
      ; span : Span.t
      }
  | For of
      { paren : for_paren
      ; body : stmt
      ; span : Span.t
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
  ; span : Span.t
  }
[@@deriving sexp_of]

and lvalue = var [@@deriving sexp_of]

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
  | Int_const of Z.t Spanned.t
  | Bool_const of bool Spanned.t
  | Var of var
  | Unary of
      { op : unary_op
      ; expr : expr
      ; span : Span.t
      }
  | Bin of
      { lhs : expr
      ; op : bin_op
      ; rhs : expr
      ; span : Span.t
      }
  | Ternary of
      { cond : expr
      ; then_expr : expr
      ; else_expr : expr
      ; span : Span.t
      }
  | Call of
      { func : var
      ; args : expr list
      ; span : Span.t
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

type param =
  { var : var
  ; ty : ty
  ; span : Span.t
  }
[@@deriving sexp_of]

type func =
  { is_extern : bool
  ; ty : ty
  ; name : var
  ; params : param list
  ; body : block option
  ; span : Span.t
  }
[@@deriving sexp_of]

type global_decl =
  | Func of func
  | Typedef of
      { ty : ty
      ; name : var
      ; span : Span.t
      }
[@@deriving sexp_of]

type program = global_decl list [@@deriving sexp_of]

let expr_span (expr : expr) =
  match expr with
  | Int_const { span; _ }
  | Bool_const { span; _ }
  | Var { span; _ }
  | Unary { span; _ }
  | Bin { span; _ }
  | Ternary { span; _ }
  | Call { span; _ } -> span
;;

let stmt_span (stmt : stmt) =
  match stmt with
  | Decl { span; _ }
  | Block { span; _ }
  | Assign { span; _ }
  | Post { span; _ }
  | If { span; _ }
  | While { span; _ }
  | Return { span; _ }
  | For { span; _ } -> span
  | Assert { span; _ } -> span
  | Effect expr -> expr_span expr
;;

let ty_span (ty : ty) =
  match ty with
  | Bool span | Int span | Void span -> span
  | Ty_var { span; _ } -> span
;;

let var v = Var v

let bin ~lhs ~op ~rhs =
  Bin { lhs; rhs; op; span = Span.Syntax.(expr_span lhs ++ expr_span rhs) }
;;

let bool_const b = Bool_const b
let nop_stmt span = Block { block = []; span }
