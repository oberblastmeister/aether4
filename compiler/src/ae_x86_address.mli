open Std

module Index : sig
  type 'r t =
    { index : 'r
    ; scale : int (* 1, 2, 4, 8 *)
    }
  [@@deriving sexp_of]

  val create : 'r -> int -> 'r t
end

module Base : sig
  type 'r t = Reg of 'r [@@deriving sexp_of, variants]
end

type 'r t =
  { base : 'r Base.t
  ; index : 'r Index.t option
  ; offset : int
  }
[@@deriving sexp_of, equal]

val create : 'r -> ?index:'r Index.t -> int -> 'r t
val iter_regs : 'r t -> 'r Iter.t
