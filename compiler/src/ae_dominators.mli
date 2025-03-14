open Std
module Entity := Ae_entity_std
module Table := Entity.Ident.Table
module Label_entity := Ae_label_entity
module Label := Label_entity.Ident
module Graph := Ae_data_graph_std

module Immediate : sig
  type t [@@deriving sexp_of]

  val find : t -> Label.t -> Label.t option
  val is_reachable : t -> Label.t -> bool
  val compute : ?node_length:int -> start:Label.t -> Label.t Graph.Bi.t -> t
end

(* A tree of labels to other labels that are immediately dominated *)
module Tree : sig
  type t [@@deriving sexp_of]

  val children : t -> Label.t -> Label.t list
  val of_immediate : Immediate.t -> t
end

module Frontier : sig
  type t [@@deriving sexp_of]

  val find : t -> Label.t -> Label.t list
  val find_iter : t -> Label.t -> Label.t Iter.t
  val compute : Immediate.t -> Label.t Graph.Bi.t -> t
end
