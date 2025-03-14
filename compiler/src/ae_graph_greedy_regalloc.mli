open Std
module Entity := Ae_entity_std
module Vreg := Ae_vreg_entity.Ident

(*
   All Vreg.t must be added to the interference graph,
  including ones that don't interfere with anything.
  
  This is why the add function exists to add a single Vreg.t.
*)
module Graph : sig
  type t [@@deriving sexp_of]

  val create : unit -> t
  val iter_vreg : t -> Vreg.t Iter.t
  val mem : t -> Vreg.t -> bool
  val add : t -> Vreg.t -> unit
  val add_edge : t -> Vreg.t -> Vreg.t -> unit
end

val color_graph : Graph.t -> Vreg.Set.t -> int Vreg.Table.t * int
