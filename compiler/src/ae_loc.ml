open Std

module T = struct
  type t =
    { line : int
    ; col : int
    ; offset : int
    }
  [@@deriving compare, equal, hash]

  let to_string t = [%string "%{t.line#Int}:%{t.col#Int}"]
  let sexp_of_t t = Sexp.Atom (to_string t)
  let t_of_sexp _s = todol [%here]
end

include T
include Base.Comparable.Make (T)

let none = { line = -1; col = -1; offset = -1 }

(* this is 1-indexed *)
let of_position (pos : Lexing.position) : t =
  Lexing.
    { line = pos.pos_lnum
    ; col = pos.pos_cnum - pos.pos_bol + 1 (* 1-indexed *)
    ; offset = pos.pos_cnum
    }
;;
