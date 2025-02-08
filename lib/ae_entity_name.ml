open Std
module Id = Ae_entity_id
module Entity_table = Ae_entity_table
module Entity_map = Ae_entity_map

module T = struct
  type 'k t =
    { name : string [@compare.ignore] [@equal.ignore] [@hash.ignore]
    ; id : 'k Id.t
    }
  [@@deriving compare, equal, hash, sexp]
end

include T

let unchecked_coerce { name; id } = { name; id = Id.unchecked_coerce id }
let create name id = { name; id }

module Arg = struct
  type nonrec 'k t = 'k t

  let sexp_of_t = sexp_of_t
  let to_int (x : _ t) = x.id
end

module Map = Entity_map.Make (Arg)
module Table = Entity_table.Make (Arg)
