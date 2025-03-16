open Std
module Id = Ae_entity_id
module Entity_table = Ae_entity_table
module Entity_set = Ae_entity_set
module Entity_map = Ae_entity_map

module T = struct
  type 'k t =
    { name : string [@compare.ignore] [@equal.ignore] [@hash.ignore]
    ; id : 'k Id.t
    ; info : Info.t option
    }
  [@@deriving compare, equal, hash, sexp]
end

include T

let to_id t = t.id
let unchecked_coerce { name; id; info } = { name; id = Id.unchecked_coerce id; info }
let create ?info name id = { name; id; info }

let fresh ?(name = "fresh") ?info gen =
  let id = !gen in
  gen := id + 1;
  { name; id; info }
;;

let freshen gen t = fresh ~name:t.name gen
let to_string name = [%string "%{name.name}@%{name.id#Int}"]
let sexp_of_t _ name = Sexp.Atom (to_string name)

module Arg = struct
  type nonrec 'k t = 'k t

  let sexp_of_t = sexp_of_t
  let to_int (x : _ t) = x.id
end

module Map = Entity_map.Make_phantom (Arg)
module Table = Entity_table.Make_phantom (Arg)
module Set = Entity_set.Make_phantom (Arg)

let add_table ?(name = "fresh") ?info data table =
  let index = Table.max_index table + 1 in
  let ident = { name; id = index; info } in
  Table.add_exn table ~key:ident ~data;
  ident
;;
