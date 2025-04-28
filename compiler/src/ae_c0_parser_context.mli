type t [@@deriving sexp_of]

val empty : t
val declare_ident : string -> t -> t
val declare_ty_ident : string -> t -> t
val is_ty_ident : t -> string -> bool
