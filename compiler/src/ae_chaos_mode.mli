module Spill_mode : sig
  type t =
    | All
    | Random
  [@@deriving sexp, equal, compare]
end

module Options : sig
  type t = { spill_mode : Spill_mode.t option }
end

val with_options : (Options.t -> Options.t) -> (unit -> 'a) -> 'a
val get_options : unit -> Options.t
