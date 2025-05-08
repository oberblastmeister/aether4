open Std
module Spanned = Ae_spanned
module Span = Ae_span

type var = string Spanned.t [@@deriving sexp_of]

type ty =
  | Int of Span.t
  | Bool of Span.t
  | Ty_var of var
  | Ty_struct of
      { name : var
      ; span : Span.t
      }
  | Void of Span.t
  | Pointer of
      { ty : ty
      ; span : Span.t
      }
  | Array of
      { ty : ty
      ; span : Span.t
      }
[@@deriving sexp_of]

type block =
  { label : var option
  ; stmts : stmt list
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
      { lvalue : expr
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
  | Break of
      { label : var option
      ; span : Span.t
      }
  | Continue of Span.t
[@@deriving sexp_of]

and for_paren =
  { init : stmt option
  ; cond : expr option
  ; incr : stmt option
  }
[@@deriving sexp_of]

and assign =
  { lvalue : expr
  ; op : assign_op
  ; expr : expr
  ; span : Span.t
  }
[@@deriving sexp_of]

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
  | Alloc of
      { ty : ty
      ; span : Span.t
      }
  | Alloc_array of
      { ty : ty
      ; expr : expr
      ; span : Span.t
      }
  | Null of Span.t
  | Var of var
  | Deref of
      { expr : expr
      ; span : Span.t
      }
  | Field_access of
      { expr : expr
      ; field : string Spanned.t
      ; span : Span.t
      ; deref : bool
      }
  | Index of
      { expr : expr
      ; index : expr
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

type field =
  { name : var
  ; ty : ty
  ; span : Span.t
  }
[@@deriving sexp_of]

type strukt =
  { fields : field list
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
  | Struct of
      { name : var
      ; strukt : strukt option
      ; span : Span.t
      }
[@@deriving sexp_of]

type program = global_decl list [@@deriving sexp_of]

let rec expr_span (expr : expr) =
  match expr with
  | Int_const { span; _ }
  | Bool_const { span; _ }
  | Unary { span; _ }
  | Bin { span; _ }
  | Ternary { span; _ }
  | Call { span; _ }
  | Var { span; _ }
  | Deref { span; _ }
  | Field_access { span; _ }
  | Null span
  | Alloc { span; _ }
  | Index { span; _ }
  | Alloc_array { span; _ } -> span
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
  | For { span; _ }
  | Break { label = _; span }
  | Continue span
  | Assert { span; _ } -> span
  | Effect expr -> expr_span expr
;;

let ty_span (ty : ty) =
  match ty with
  | Ty_struct { span; _ }
  | Bool span
  | Int span
  | Void span
  | Pointer { span; _ }
  | Array { span; _ }
  | Ty_var { span; _ } -> span
;;

let var v = Var v

let bin ~lhs ~op ~rhs =
  Bin { lhs; rhs; op; span = Span.Syntax.(expr_span lhs ++ expr_span rhs) }
;;

let bool_const b = Bool_const b
let nop_stmt span = Block { label = None; stmts = []; span }
