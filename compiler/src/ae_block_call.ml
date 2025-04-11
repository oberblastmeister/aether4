open Std

open struct
  module Entity = Ae_entity_std
  module Label_entity = Ae_label_entity
  module Label = Label_entity.Ident
end

module type Location = sig
  type t [@@deriving sexp_of]
end

module type S = sig
  module Location : Location

  type t =
    { label : Label.t
    ; args : Location.t list
    }
  [@@deriving sexp_of]

  val create : Label.t -> Location.t list -> t
  val label : t -> Label.t
  val add_args : t -> Location.t list -> t
  val iter_uses : t -> Location.t Iter.t
  val map_uses : t -> f:(Location.t -> Location.t) -> t
end

module Make_S (Location : Location) = struct
  module type S = S with module Location = Location
end

module Make (Location : Location) : Make_S(Location).S = struct
  module Location = Location

  type t =
    { label : Label.t
    ; args : Location.t list
    }
  [@@deriving sexp_of]

  let create label args = { label; args }
  let label t = t.label
  let add_args t args = { t with args = t.args @ args }
  let iter_uses t ~f = List.iter t.args ~f
  let map_uses t ~f = { t with args = List.map t.args ~f }
end
