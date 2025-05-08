open Std
module Span := Ae_span

type 'a t =
  { t : 'a
  ; span : Span.t
  }
[@@deriving sexp, compare, equal, hash]

val create : 'a -> Span.t -> 'a t
val map : 'a t -> f:('a -> 'b) -> 'b t
val map_option : 'a t -> f:('a -> 'b option) -> 'b t option
val value : 'a t -> 'a
val none : 'a -> 'a t
