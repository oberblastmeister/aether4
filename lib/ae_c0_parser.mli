open Std
module Token := Ae_c0_token
module Cst = Ae_c0_cst

module Error : sig
  type t = Sexp of Sexp.t
end

val parse : Token.t list -> (Cst.program, Error.t) result
