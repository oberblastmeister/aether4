open Std
module Entity := Ae_entity_std
module Temp := Ae_abs_asm_temp_entity.Ident

(*
   All Temp.t must be added to the interference graph,
  including ones that don't interfere with anything.
  
  This is why the add function exists to add a single Temp.t.
*)
module Graph : sig
  type t [@@deriving sexp_of]

  val create : unit -> t
  val iter_temp : t -> Temp.t Iter.t
  val mem : t -> Temp.t -> bool
  val add : t -> Temp.t -> unit
  val add_edge : t -> Temp.t -> Temp.t -> unit
end

val color_graph : Graph.t -> Temp.Set.t -> int Temp.Table.t * int
