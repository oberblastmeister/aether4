(* TODO: fix this code *)

(* open Std
module Temp_entity = Ae_abs_asm_temp_entity

module Id = Entity.Id
module Temp = Temp_entity.Ident
module Bitvec = Ae_data_bitvec

type t =
  { matrix : Bitvec.t
  ; nodes : Bitvec.t
  ; size : int
  }
[@@deriving sexp_of]

let create ~size =
  let matrix = Bitvec.create ~size:(size * size) () in
  let nodes = Bitvec.create ~size () in
  { matrix; nodes; size }
;;

let add t v = Bitvec.add t.nodes (Id.to_int v)

let add_directed_edge t v1 v2 =
  Bitvec.add t.matrix ((t.size * Id.to_int v1) + Id.to_int v2)
;;

let add_edge t v1 v2 =
  add t v1;
  add t v2;
  add_directed_edge t v1 v2;
  add_directed_edge t v2 v1
;;

let mem t v = Bitvec.mem t.nodes (Id.to_int v)
let iter_nodes t ~f = Bitvec.iter t.nodes ~f:(fun i -> f (Id.unchecked_of_int i))

let iter_neighbors t v ~f =
  let k = Id.to_int v in
  for i = t.size * k to (t.size * k) + t.size - 1 do
    if Bitvec.mem t.matrix i then f (Id.unchecked_of_int (i - (t.size * k)))
  done
;; *)
