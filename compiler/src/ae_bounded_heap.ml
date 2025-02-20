open Std
module Entity = Ae_entity_std
module Id = Entity.Id
module Vreg_entity = Ae_vreg_entity
module Vreg_id = Vreg_entity.Id
module Table = Id.Table
open Id.Table.Syntax

(* entities form a doubly linked list *)
(* heap points to the start of the linked list *)
module Entry = struct
  type t =
    { next : Vreg_id.t option
    ; prev : Vreg_id.t option
    ; weight : int (* if weight is -1 then entry is not in heap *)
    }
  [@@deriving sexp_of]

  let create ?next ?prev weight = { next; prev; weight }
end

type t =
  { heap : Vreg_id.t Option_array.t
  ; links : Entry.t Vreg_id.Table.t
  ; mutable min_weight : int
  }
[@@deriving sexp_of]

module Doubly_linked = Entity.Doubly_linked.Make (struct
    module Witness = Vreg_entity.Witness

    module Data = struct
      type t = Entry.t

      let next t = t.Entry.next
      let prev t = t.Entry.prev
      let set_next t next = { t with Entry.next }
      let set_prev t prev = { t with Entry.prev }
    end
  end)

let create ?entity_size ~weight_bound () =
  { heap = Option_array.create ~len:weight_bound
  ; links = Id.Table.create ?size:entity_size ()
  ; min_weight = 0
  }
;;

let add_exn t vreg weight =
  if Id.Table.mem t.links vreg && t.links.!(vreg).weight <> -1
  then raise_s [%message "Cannot add vreg twice" (vreg : Vreg_id.t)];
  if weight < t.min_weight
  then
    raise_s
      [%message
        "Cannot add weight less than min weight, weights must be monotonically increasing"
          (weight : int)
          (t.min_weight : int)];
  if Id.Table.mem t.links vreg
  then (
    t.links.!(vreg) <- { (t.links.!(vreg)) with weight };
    ())
  else t.links.!(vreg) <- Entry.create weight;
  (match Option_array.get t.heap weight with
   | None -> ()
   | Some vreg_start ->
     Doubly_linked.insert_head t.links vreg ~head:vreg_start;
     ());
  Option_array.set_some t.heap weight vreg;
  ();
  ()
;;

let remove_max t =
  while
    t.min_weight < Option_array.length t.heap && Option_array.is_none t.heap t.min_weight
  do
    t.min_weight <- t.min_weight + 1;
    ()
  done;
  if t.min_weight >= Option_array.length t.heap
  then None
  else
    Some
      (let vreg_start = Option_array.get_some_exn t.heap t.min_weight in
       Option_array.set t.heap t.min_weight t.links.!(vreg_start).next;
       Doubly_linked.remove_head t.links vreg_start;
       t.links.!(vreg_start) <- { (t.links.!(vreg_start)) with weight = -1 };
       vreg_start, t.min_weight)
;;

let remove_exn t vreg =
  if (not (Id.Table.mem t.links vreg)) || t.links.!(vreg).weight = -1
  then raise_s [%message "Cannot remove vreg that is not in heap" (vreg : Vreg_id.t)];
  let curr_weight = t.links.!(vreg).weight in
  let vreg_start = Option_array.get_some_exn t.heap curr_weight in
  if Vreg_id.equal vreg_start vreg
  then (
    Option_array.set t.heap curr_weight t.links.!(vreg).next;
    Doubly_linked.remove_head t.links vreg;
    ())
  else (
    Doubly_linked.remove t.links vreg;
    ());
  t.links.!(vreg) <- { (t.links.!(vreg)) with weight = -1 };
  ()
;;

let increase_exn t vreg by =
  if by < 0 then raise_s [%message "Cannot increase weight by negative amount" (by : int)];
  let curr_weight = t.links.!(vreg).weight in
  remove_exn t vreg;
  add_exn t vreg (curr_weight + by);
  ()
;;

let mem t vreg = Id.Table.mem t.links vreg && t.links.!(vreg).weight <> -1
