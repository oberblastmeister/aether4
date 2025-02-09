open Std
module Entity := Ae_entity_std
module Vreg := Ae_vreg_entity.Name

(*
   All Vreg.t must be added to the interference graph,
  including ones that don't interfere with anything.
  
  This is why the add function exists to add a single Vreg.t.
*)
module Graph : sig
  type t

  val create : unit -> t
  val add : t -> Vreg.t -> unit
  val add_edge : t -> Vreg.t -> Vreg.t -> unit
end

module Color_entity : Entity.S
module Color = Color_entity.Id

val color_graph : Graph.t -> Vreg.Set.t -> Color.t Vreg.Table.t
