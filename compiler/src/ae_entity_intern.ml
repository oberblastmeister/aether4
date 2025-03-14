open Std
module Ident = Ae_entity_ident

module String_to_name = struct
  type 'w t =
    { map : 'w Ident.t String.Table.t
    ; mutable id : int
    }
  [@@deriving sexp_of]

  let create () = { map = String.Table.create (); id = 0 }

  let intern (t : _ t) key =
    Hashtbl.find_or_add t.map key ~default:(fun () ->
      let id = t.id in
      let name : _ Ident.t = { name = key; id } in
      t.id <- id + 1;
      name)
  ;;

  let fresh ?(name = "fresh") t =
    let id = t.id in
    t.id <- id + 1;
    Ident.{ name; id }
  ;;

  let find_exn t key = Hashtbl.find_exn t.map key
  let next_id t = t.id

  module Make_global (Witness : T) () = struct
    let t : Witness.t t = create ()
    let fresh ?name () = fresh ?name t
    let intern k = intern t k
    let find_exn k = find_exn t k
    let next_id () = next_id t
  end
end

module Ident_to_name = struct
  type ('w1, 'w2) t =
    { map : ('w1, 'w2 Ident.t) Ident.Table.t
    ; mutable id : int
    }

  let intern (t : ('w1, 'w2) t) key =
    Ident.Table.find_or_add t.map key ~default:(fun () ->
      let id = t.id in
      let name : _ Ident.t = { name = key.name; id } in
      t.id <- id + 1;
      name)
  ;;

  let find_exn t key = Ident.Table.find_exn t.map key
end
