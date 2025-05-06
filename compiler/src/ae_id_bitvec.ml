open Std
open Ae_intable

open struct
  module Bitvec = Ae_data_bitvec
end

module type S = sig
  module Key : Intable

  type t [@@deriving sexp_of]

  val create : ?size:int -> ?default:bool -> unit -> t
  val add : t -> Key.t -> unit
  val remove : t -> Key.t -> unit
  val mem : t -> Key.t -> bool
  val to_bitvec : t -> Bitvec.t
end

module Make (Key : Intable) : S with module Key = Key = struct
  module Key = Key

  type t = Bitvec.t [@@deriving sexp_of]

  let create = Bitvec.create
  let add t k = Bitvec.add t (Key.to_int k)
  let remove t k = Bitvec.remove t (Key.to_int k)
  let mem t k = Bitvec.mem t (Key.to_int k)
  let to_bitvec t = t
end
