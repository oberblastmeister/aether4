open Std

module type Intable = sig
  type t

  val to_int : t -> int
end

module type Mutable_map = sig
  type 'a t

  module Key : sig
    type t
  end

  val create : unit -> 'a t
  val find : 'a t -> Key.t -> 'a option
  val set : 'a t -> key:Key.t -> data:'a -> unit
  val remove : 'a t -> Key.t -> unit
  val mem : 'a t -> Key.t -> bool
end

module type Mutable_set = sig
  type t

  module Key : sig
    type t
  end

  val create : unit -> t
  val add : t -> Key.t -> unit
  val remove : t -> Key.t -> unit
  val mem : t -> Key.t -> bool
end
