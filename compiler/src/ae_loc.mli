open Std

type t =
  { line : int
  ; col : int
  ; offset : int
  }
[@@deriving sexp, compare, equal, hash]

include Base.Comparable.S with type t := t

val none : t
val of_position : Lexing.position -> t
val to_string : t -> string
