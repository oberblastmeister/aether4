open Std
module Bag = Ae_data_bag
open Bag.Syntax

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

module Poly : S_poly = struct
  type ('w, 'a) t = 'w Bag.t * 'a

  let write_list l = Bag.of_list l, ()
  let pure x = Bag.empty, x

  let bind m f =
    let ws, a = m in
    let ws', b = f a in
    ws ++ ws', b
  ;;

  let both m1 m2 =
    let ws1, a = m1 in
    let ws2, b = m2 in
    ws1 ++ ws2, (a, b)
  ;;

  let map f m =
    let ws, x = m in
    ws, f x
  ;;

  module Syntax = struct
    let ( let* ) = bind
    let ( and+ ) = both
    let ( let+ ) m f = map f m
  end
end

module type S = sig
  type w

  include S_poly

  type nonrec 'a t = (w, 'a) t
end

module Make (W : T) = struct
  type w = W.t

  include Poly

  type nonrec 'a t = (w, 'a) t
end
