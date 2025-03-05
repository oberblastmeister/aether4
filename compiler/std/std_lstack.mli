open Core

type 'a t

val create : unit -> 'a t
val push : 'a t -> 'a -> unit
val pop : 'a t -> 'a option
val append_list : 'a t -> 'a list -> unit
val pop_exn : 'a t -> 'a
val to_list : 'a t -> 'a list
val to_list_rev : 'a t -> 'a list
