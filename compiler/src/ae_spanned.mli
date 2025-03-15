open Std
module Span := Ae_span

type 'a t =
  { t : 'a
  ; span : Span.t
  }
[@@deriving sexp, compare, equal, hash]

val value : 'a t -> 'a
