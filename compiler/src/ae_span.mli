open Std
module Loc := Ae_loc

type t =
  { start : Loc.t
  ; stop : Loc.t
  }
[@@deriving sexp, compare, equal, hash]

val combine : t -> t -> t

val of_positions : start:Lexing.position -> stop:Lexing.position -> t