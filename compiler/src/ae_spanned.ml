open Std
module Span = Ae_span

type 'a t =
  { t : 'a
  ; span : Span.t
  }
[@@deriving sexp, compare, equal, hash]

let map t ~f = { t with t = f t.t }

let map_option t ~f =
  match f t.t with
  | None -> None
  | Some x -> Some { t with t = x }
;;

let value t = t.t
