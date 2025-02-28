module type Key_phantom = sig
  type 'w t [@@deriving sexp_of]

  val to_int : 'w t -> int
end

module type Key = sig
  type t [@@deriving sexp_of]

  val to_int : t -> int
end