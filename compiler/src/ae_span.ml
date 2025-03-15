open Std
module Loc = Ae_loc

type t =
  { start : Loc.t
  ; stop : Loc.t
  }
[@@deriving sexp, compare, equal, hash]

let none = { start = Loc.none; stop = Loc.none }

let combine span1 span2 =
  { start = Loc.min span1.start span2.start; stop = Loc.max span1.stop span2.stop }
;;

let of_positions ~start ~stop =
  { start = Loc.of_position start; stop = Loc.of_position stop }
;;

module Syntax = struct
  let ( ++ ) = combine
end
