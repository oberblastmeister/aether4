module Make (Arg : sig
    module Temp : sig
      type t [@@deriving sexp_of]
    end

    module Ty : sig
      type t [@@deriving sexp_of]
    end
  end) : sig
  open Arg

  module Move : sig
    type t =
      { dst : Temp.t
      ; src : Temp.t
      ; ty : Ty.t
      }
    [@@deriving sexp_of]
  end

  val sequentialize
    :  in_same_reg:(Temp.t -> Temp.t -> bool)
    -> get_scratch:(unit -> Temp.t)
    -> Move.t list
    -> Move.t list * bool
end
