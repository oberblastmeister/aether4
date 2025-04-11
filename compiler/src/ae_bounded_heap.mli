module Temp_entity := Ae_abs_asm_temp_entity
module Temp_id := Temp_entity.Id

(* weights must be monotonically increasing, so can only increase by positive amount *)
type t [@@deriving sexp_of]

val create : ?entity_size:int -> weight_bound:int -> unit -> t

(* precondition, id must not already be added. adds id to the heap *)
val add_exn : t -> Temp_id.t -> int -> unit
val remove_max : t -> (Temp_id.t * int) option

(* precondition, id must be on the heap *)
val remove_exn : t -> Temp_id.t -> unit

(* precondition, id must be on the heap. amount must be positive *)
val increase_exn : t -> Temp_id.t -> int -> unit
val mem : t -> Temp_id.t -> bool
