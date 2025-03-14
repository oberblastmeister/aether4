open Std
module Label := Ae_label_entity.Ident
module Tir := Ae_tir_types
module Temp := Tir.Temp

val convert : Tir.Func.t -> Tir.Func.t
val compute_phi_placements : Tir.Func.t -> Temp.t list Label.Table.t
