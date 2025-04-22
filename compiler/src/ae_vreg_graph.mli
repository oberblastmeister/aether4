(* open Std
module Temp_entity := Ae_abs_asm_temp_entity
module Temp_id := Temp_entity.Id

type t

val create : size:int -> t
val mem : t -> Temp_id.t -> bool
val iter_nodes : t -> Temp_id.t Iter.t
val iter_neighbors : t -> Temp_id.t -> Temp_id.t Iter.t *)
