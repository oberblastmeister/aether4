module type S = sig
  type t [@@deriving sexp_of]
end
