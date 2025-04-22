open Std
module Temp := Ae_temp

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

val color_graph
  :  spilled_color:int
  -> available_colors:Int.Set.t
  -> graph:Graph.t
  -> precolored:int Temp.Map.t
  -> int Temp.Table.t * Int.Set.t
