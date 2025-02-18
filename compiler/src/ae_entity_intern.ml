open Std
module Name = Ae_entity_name

module String_to_name = struct
  type 'w t =
    { map : 'w Name.t String.Table.t
    ; mutable id : int
    }
  [@@deriving sexp_of]

  let create () = { map = String.Table.create (); id = 0 }

  let intern (t : _ t) key =
    Hashtbl.find_or_add t.map key ~default:(fun () ->
      let id = t.id in
      let name : _ Name.t = { name = key; id } in
      t.id <- id + 1;
      name)
  ;;

  let find_exn t key = Hashtbl.find_exn t.map key

  module Make_global (Witness : T) () = struct
    let t : Witness.t t = create ()
    let intern k = intern t k
    let find_exn k = find_exn t k
  end
end

module Name_to_name = struct
  type ('w1, 'w2) t =
    { map : ('w1, 'w2 Name.t) Name.Table.t
    ; mutable id : int
    }

  let intern (t : ('w1, 'w2) t) key =
    Name.Table.find_or_add t.map key ~default:(fun () ->
      let id = t.id in
      let name : _ Name.t = { name = key.name; id } in
      t.id <- id + 1;
      name)
  ;;

  let find_exn t key = Name.Table.find_exn t.map key
end
