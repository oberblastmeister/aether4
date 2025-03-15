open Std

module T = struct
  type t =
    { line : int
    ; col : int
    ; offset : int
    }
  [@@deriving sexp, compare, equal, hash]
end

include T
include Base.Comparable.Make (T)

(* this is 1-indexed *)
let of_position (pos : Lexing.position) : t =
  Lexing.
    { line = pos.pos_lnum
    ; col = pos.pos_cnum - pos.pos_bol + 1 (* 1-indexed *)
    ; offset = pos.pos_cnum
    }
;;
