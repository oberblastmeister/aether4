open Std

open struct
  module Entity = Ae_entity_std
  module Label_entity = Ae_label_entity
  module Label = Label_entity.Ident
end

module Make (Temp_entity : Entity.S) = struct
  type t =
    { label : Label.t
    ; args : Temp_entity.Ident.t list
    }
  [@@deriving sexp_of]

  let create ?(args = []) label = { label; args }
  let iter_uses t ~f = List.iter t.args ~f
  let map_uses t ~f = { t with args = List.map t.args ~f }
end
