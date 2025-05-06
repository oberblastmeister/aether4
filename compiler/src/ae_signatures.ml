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

  val create : ?growth_allowed:bool -> ?size:int -> unit -> 'a t
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

  val create : ?growth_allowed:bool -> ?size:int -> unit -> t
  val add : t -> Key.t -> unit
  val remove : t -> Key.t -> unit
  val mem : t -> Key.t -> bool
end

module type Semigroup = sig
  type t

  val append : t -> t -> t
end

module type Monoid = sig
  type t

  val empty : t

  include Semigroup with type t := t
end
