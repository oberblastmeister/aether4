open Std
module Loc = Ae_loc

type t =
  { start : Loc.t
  ; stop : Loc.t
  }
[@@deriving compare, equal, hash]

let none = { start = Loc.none; stop = Loc.none }

let combine span1 span2 =
  { start = Loc.min span1.start span2.start; stop = Loc.max span1.stop span2.stop }
;;

let of_positions ~start ~stop =
  { start = Loc.of_position start; stop = Loc.of_position stop }
;;

let to_string t =
  if Loc.equal t.start t.stop
  then Loc.to_string t.start
  else if t.start.line = t.stop.line
  then
    if t.start.col + 1 = t.stop.col
    then [%string "%{t.start.line#Int}:%{t.start.col#Int}"]
    else [%string "%{t.start.line#Int}:%{t.start.col#Int}-%{t.stop.col#Int}"]
  else
    [%string
      "[%{t.start.line#Int},%{t.start.col#Int}]-[%{t.stop.line#Int},%{t.stop.col#Int}]"]
;;

let sexp_of_t t = Sexp.Atom (to_string t)
let to_info t = Info.create_s (sexp_of_t t)
let t_of_sexp _s = todol [%here]

module Syntax = struct
  let ( ++ ) = combine
end
