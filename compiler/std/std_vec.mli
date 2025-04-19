module Iter := Functional.Iter

type 'a t

val create : unit -> 'a t
val init : int -> (int -> 'a) -> 'a t
val create : unit -> 'a t
val push : 'a t -> 'a -> unit
val get : 'a t -> int -> 'a
val length : 'a t -> int
val iter : 'a t -> 'a Iter.t
val append_iter : 'a t -> 'a Iter.t -> unit
val append_list : 'a t -> 'a list -> unit
val append : 'a t -> 'a t -> unit
val to_list : 'a t -> 'a list
val rev : 'a t -> 'a t
