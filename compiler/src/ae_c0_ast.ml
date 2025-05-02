(* the ty field should be set to None until we do typechecking which fills it in *)
open Std
module Span = Ae_span
module Spanned = Ae_spanned

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

type ty =
  | Int of Span.t
  | Bool of Span.t
  | Void of Span.t
  | Ty_var of var
  | Pointer of
      { ty : ty
      ; span : Span.t
      }
[@@deriving sexp_of]

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
      { expr : expr option
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
  | Assert of
      { expr : expr
      ; span : Span.t
      }
[@@deriving sexp_of]

and lvalue = var [@@deriving sexp_of]
and block = stmt list [@@deriving sexp_of]
and nullary_op = Alloc of ty
and unary_op = Deref

and expr =
  | Var of
      { var : var
      ; ty : ty option
      }
  | Int_const of int64 Spanned.t
  | Bool_const of bool Spanned.t
  | Nullary of
      { op : nullary_op
      ; span : Span.t
      }
  | Ternary of
      { cond : expr
      ; then_expr : expr
      ; else_expr : expr
      ; ty : ty option
      ; span : Span.t
      }
  | Unary of
      { expr : expr
      ; op : unary_op
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
  | Call of
      { func : var
      ; args : expr list
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

type param =
  { var : var
  ; ty : ty
  ; span : Span.t
  }
[@@deriving sexp_of]

type func_sig =
  { ty : ty
  ; params : param list
  ; is_extern : bool (* Controls whether we use the c calling convention *)
  ; span : Span.t
  }
[@@deriving sexp_of]

type func_defn =
  { ty : ty
  ; name : var
  ; params : param list
  ; body : block
  ; span : Span.t
  }
[@@deriving sexp_of]

type global_decl =
  | Extern_func_defn of
      { name : var
      ; ty : func_sig
      }
  | Func_decl of
      { name : var
      ; ty : func_sig
      }
  | Func_defn of func_defn
  | Typedef of
      { ty : ty
      ; name : var
      ; span : Span.t
      }
[@@deriving sexp_of]

type program = global_decl list [@@deriving sexp_of]

let func_defn_to_ty func =
  { ty = func.ty; params = func.params; span = func.span; is_extern = false }
;;

let bool_ty = Bool Span.none
let int_ty = Int Span.none
let void_ty = Void Span.none

let expr_span = function
  | Ternary { span; _ }
  | Var { var = { span; _ }; _ }
  | Bin { span; _ }
  | Nullary { span; _ }
  | Call { span; _ }
  | Int_const { span; _ }
  | Bool_const { span; _ } -> span
  | Unary { span; _ } -> span
;;

let expr_ty_exn = function
  | Ternary { ty; _ } | Var { ty; _ } | Bin { ty; _ } | Call { ty; _ } ->
    Option.value_exn ty
  | Int_const _ -> Int Span.none
  | Bool_const _ -> Bool Span.none
  | Nullary { op = Alloc ty; span } -> Pointer { ty; span }
  | Unary { ty; _ } -> Option.value_exn ty
;;

let nop_stmt span = Block { block = []; span }

let get_func_ty_map program =
  List.filter_map program ~f:(fun decl ->
    match decl with
    | Extern_func_defn { name; ty } -> Some (name, ty)
    | Func_decl { name; ty } -> Some (name, ty)
    | Func_defn ({ name; _ } as defn) -> Some (name, func_defn_to_ty defn)
    | Typedef _ -> None)
  |> Var.Map.of_alist_exn
;;
