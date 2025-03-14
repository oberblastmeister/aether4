open Std

type t [@@deriving sexp]

val create : ?size:int -> ?default:bool -> unit -> t
val add : t -> int -> unit
val remove : t -> int -> unit
val of_list : int list -> t
val mem : t -> int -> bool
val to_list : t -> int list
val of_iter : int Iter.t -> t
val iter : t -> int Iter.t
