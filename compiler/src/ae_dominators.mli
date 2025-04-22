open Std
module Graph := Ae_data_graph_std
module Label := Ae_label

type t [@@deriving sexp_of]

(* none if not reachable from the start label *)
val find : t -> Label.t -> Label.t option
val is_reachable : t -> Label.t -> bool
val compute : ?node_length:int -> start:Label.t -> Label.t Graph.Bi.t -> t
val dominates : t -> higher:Label.t -> lower:Label.t -> bool

type dominators := t

(* A tree of labels to other labels that are immediately dominated *)
module Tree : sig
  type t [@@deriving sexp_of]

  val children : t -> Label.t -> Label.t list
  val of_immediate : dominators -> t
end

module Frontier : sig
  type t [@@deriving sexp_of]

  val find : t -> Label.t -> Label.t list
  val find_iter : t -> Label.t -> Label.t Iter.t
  val compute : dominators -> Label.t Graph.Bi.t -> t
end
