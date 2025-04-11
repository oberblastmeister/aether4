open Std
module Vreg_entity := Ae_vreg_entity
module Vreg_id := Vreg_entity.Id

type t

val create : size:int -> t
val mem : t -> Vreg_id.t -> bool
val iter_nodes : t -> Vreg_id.t Iter.t
val iter_neighbors : t -> Vreg_id.t -> Vreg_id.t Iter.t
