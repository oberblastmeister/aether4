open Std

type t =
  { line : int
  ; col : int
  ; offset : int
  }
[@@deriving sexp, compare, equal, hash]

include Base.Comparable.S with type t := t

val of_position : Lexing.position -> t
