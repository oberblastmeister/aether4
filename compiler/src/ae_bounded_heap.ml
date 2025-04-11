open Std
module Entity = Ae_entity_std
module Id = Entity.Id
module Temp_entity = Ae_abs_asm_temp_entity
module Temp_id = Temp_entity.Id
module Table = Id.Table
open Id.Table.Syntax

(* entities form a doubly linked list *)
(* heap points to the start of the linked list *)
module Entry = struct
  type t =
    { next : Temp_id.t option
    ; prev : Temp_id.t option
    ; weight : int (* if weight is -1 then entry is not in heap *)
    }
  [@@deriving sexp_of]

  let create ?next ?prev weight = { next; prev; weight }
end

type t =
  { heap : Temp_id.t Option_array.t
  ; links : Entry.t Temp_id.Table.t
  ; mutable min_weight : int
  }
[@@deriving sexp_of]

module Doubly_linked = Entity.Doubly_linked.Make (struct
    module Witness = Temp_entity.Witness

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

let add_exn t temp weight =
  if Id.Table.mem t.links temp && t.links.!(temp).weight <> -1
  then raise_s [%message "Cannot add temp twice" (temp : Temp_id.t)];
  if weight < t.min_weight
  then
    raise_s
      [%message
        "Cannot add weight less than min weight, weights must be monotonically increasing"
          (weight : int)
          (t.min_weight : int)];
  if Id.Table.mem t.links temp
  then (
    t.links.!(temp) <- { (t.links.!(temp)) with weight };
    ())
  else t.links.!(temp) <- Entry.create weight;
  (match Option_array.get t.heap weight with
   | None -> ()
   | Some temp_start ->
     Doubly_linked.insert_head t.links temp ~head:temp_start;
     ());
  Option_array.set_some t.heap weight temp;
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
      (let temp_start = Option_array.get_some_exn t.heap t.min_weight in
       Option_array.set t.heap t.min_weight t.links.!(temp_start).next;
       Doubly_linked.remove_head t.links temp_start;
       t.links.!(temp_start) <- { (t.links.!(temp_start)) with weight = -1 };
       temp_start, t.min_weight)
;;

let remove_exn t temp =
  if (not (Id.Table.mem t.links temp)) || t.links.!(temp).weight = -1
  then raise_s [%message "Cannot remove temp that is not in heap" (temp : Temp_id.t)];
  let curr_weight = t.links.!(temp).weight in
  let temp_start = Option_array.get_some_exn t.heap curr_weight in
  if Temp_id.equal temp_start temp
  then (
    Option_array.set t.heap curr_weight t.links.!(temp).next;
    Doubly_linked.remove_head t.links temp;
    ())
  else (
    Doubly_linked.remove t.links temp;
    ());
  t.links.!(temp) <- { (t.links.!(temp)) with weight = -1 };
  ()
;;

let increase_exn t temp by =
  if by < 0 then raise_s [%message "Cannot increase weight by negative amount" (by : int)];
  let curr_weight = t.links.!(temp).weight in
  remove_exn t temp;
  add_exn t temp (curr_weight + by);
  ()
;;

let mem t temp = Id.Table.mem t.links temp && t.links.!(temp).weight <> -1
