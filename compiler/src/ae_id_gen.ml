open Std

type t =
  { first_id : int
  ; mutable next_id : int
  }
[@@deriving sexp_of]

let create next_id = { first_id = next_id; next_id }

let get t =
  let id = t.next_id in
  t.next_id <- id + 1;
  id
;;
