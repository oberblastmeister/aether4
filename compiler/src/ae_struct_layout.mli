open Std

module Field : sig
  type t =
    { size : int
    ; align : int
    }
  [@@deriving sexp_of]
end

type t =
  { offsets : int iarray
  ; size : int
  ; align : int
  }
[@@deriving sexp_of]

val calculate : Field.t list -> t
