open Std

module type Key = sig
  type 'w t [@@deriving sexp_of]

  val to_int : 'w t -> int
end

module type S = sig
  module Key : Key

  type ('w, 'a) t

  val create : ?size:int -> unit -> ('w, 'v) t
  val find : ('w, 'v) t -> 'w Key.t -> 'v option
  val remove : ('w, 'v) t -> 'w Key.t -> unit
  val find_exn : ('w, 'v) t -> 'w Key.t -> 'v
  val find_or_add : ('w, 'v) t -> 'w Key.t -> default:(unit -> 'v) -> 'v
  val set : ('w, 'v) t -> key:'w Key.t -> data:'v -> unit
  val add_exn : ('w, 'v) t -> key:'w Key.t -> data:'v -> unit
  val mem : ('w, 'v) t -> 'w Key.t -> bool
  val update : ('w, 'v) t -> 'w Key.t -> f:('v option -> 'v) -> unit
  val of_list : ('w Key.t * 'v) list -> ('w, 'v) t
  val of_iter : ?size:int -> ('w Key.t * 'v) Iter.t -> ('w, 'v) t
end
