type t =
  { verbose : bool
  ; path : string
  }
[@@deriving sexp_of]

val with_args : (t -> unit) -> unit
