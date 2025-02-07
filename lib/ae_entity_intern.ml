open Std
module Signatures = Ae_signatures
module Name = Ae_entity_name

module Name_to_name = struct
  type ('w1, 'w2) t =
    { map : ('w1, 'w2 Name.t) Name.Table.t
    ; mutable id : int
    }

  let intern (t : ('w1, 'w2) t) key =
    match Name.Table.find t.map key with
    | None ->
      let id = t.id in
      let name : _ Name.t = { name = key.name; id } in
      Name.Table.set t.map ~key ~data:name;
      t.id <- id + 1;
      name
    | Some name -> name
  ;;

  let find_exn t key = Name.Table.find_exn t.map key
end
