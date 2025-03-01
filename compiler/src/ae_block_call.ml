open Std

open struct
  module Entity = Ae_entity_std
  module Label_entity = Ae_label_entity
  module Label = Label_entity.Ident
end

module type S = sig
  module Temp_entity : Entity.S
  module Temp := Temp_entity.Ident

  type t =
    { label : Label.t
    ; args : Temp.t list
    }
  [@@deriving sexp_of]

  val create : ?args:Temp.t list -> Label.t -> t
  val iter_uses : t -> Temp.t Iter.t
  val map_uses : t -> f:(Temp.t -> Temp.t) -> t
end

module Make_S (Temp_entity : Entity.S) = struct
  module type S = S with module Temp_entity := Temp_entity
end

module Make (Temp_entity : Entity.S) : Make_S(Temp_entity).S = struct
  module Temp = Temp_entity.Ident

  type t =
    { label : Label.t
    ; args : Temp.t list
    }
  [@@deriving sexp_of]

  let create ?(args = []) label = { label; args }
  let iter_uses t ~f = List.iter t.args ~f
  let map_uses t ~f = { t with args = List.map t.args ~f }
end
