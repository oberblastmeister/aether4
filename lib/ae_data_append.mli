open Std

type 'a t [@@deriving sexp_of]

val of_list : 'a list -> 'a t
val ( +> ) : 'a t -> 'a list -> 'a t
val ( ++ ) : 'a t -> 'a t -> 'a t
val to_list : 'a t -> 'a list

val concat : 'a t list -> 'a t
val empty : 'a t