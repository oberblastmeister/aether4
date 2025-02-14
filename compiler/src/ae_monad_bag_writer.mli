open Std
module Bag := Ae_data_bag

module type S_poly = sig
  type ('w, 'a) t = 'w Bag.t * 'a

  val write_list : 'w list -> ('w, unit) t
  val pure : 'a -> ('w, 'a) t
  val bind : ('w, 'a) t -> ('a -> ('w, 'b) t) -> ('w, 'b) t
  val both : ('w, 'a) t -> ('w, 'b) t -> ('w, 'a * 'b) t
  val map : ('a -> 'b) -> ('w, 'a) t -> ('w, 'b) t

  module Syntax : sig
    val ( let* ) : ('w, 'a) t -> ('a -> ('w, 'b) t) -> ('w, 'b) t
    val ( let+ ) : ('w, 'a) t -> ('a -> 'b) -> ('w, 'b) t
    val ( and+ ) : ('w, 'a) t -> ('w, 'b) t -> ('w, 'a * 'b) t
  end
end

module Poly : S_poly

module type S = sig
  type w

  include S_poly

  type nonrec 'a t = (w, 'a) t
end

module Make (W : T) : S with type w = W.t
