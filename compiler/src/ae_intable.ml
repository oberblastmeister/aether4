module type Intable = sig
  type t [@@deriving sexp_of]

  val to_int : t -> int
end
