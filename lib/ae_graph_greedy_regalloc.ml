open Std
module Entity = Ae_entity_std
module Vreg_entity = Ae_vreg_entity
module Vreg = Ae_vreg_entity.Name
module Vreg_id = Vreg_entity.Id
module Id = Entity.Id
module Table = Entity.Name.Table
module Set = Entity.Name.Set
open Table.Syntax

module Interference = struct
  type t = Vreg.Set.t Vreg.Table.t

  let create () = Table.create ()
  let add (t : t) v = Table.find_or_add t v ~default:(fun () -> Set.singleton v) |> ignore

  let add_edge (t : t) v1 v2 =
    Table.update t v1 ~f:(function
      | None -> Set.singleton v2
      | Some set -> Set.add set v2);
    Table.update t v2 ~f:(function
      | None -> Set.singleton v1
      | Some set -> Set.add set v1);
    ()
  ;;
end

module Color_entity = Entity.Make ()
module Color = Color_entity.Id

(* weights must be monotonically increasing, so can only increase by positive amount *)
module Bounded_heap : sig
  type t

  val create : ?entity_size:int -> weight_bound:int -> unit -> t
  val add_exn : t -> Vreg_id.t -> int -> unit
  val remove_max : t -> Vreg_id.t option
  val remove : t -> Vreg_id.t -> unit
  val increase : t -> Vreg_id.t -> int -> unit
end = struct
  module Table = Id.Table
  open Id.Table.Syntax

  (* entities form a doubly linked list *)
  (* heap points to the start of the linked list *)
  module Entry = struct
    type t =
      { next : Vreg_id.t option
      ; prev : Vreg_id.t option
      ; weight : int
      }

    let create ?next ?prev weight = { next; prev; weight }
  end

  type t =
    { heap : Vreg_id.t Option_array.t
    ; links : Entry.t Vreg_id.Table.t
    ; mutable min_weight : int
    }

  module Doubly_linked = Entity.Doubly_linked.Make (struct
      module Witness = Vreg_entity.Id.Witness

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
    if Id.Table.mem t.links vreg
    then raise_s [%message "vreg already added!" (vreg : Vreg_id.t)]
    else (
      Id.Table.set t.links ~key:vreg ~data:(Entry.create weight);
      (match Option_array.get t.heap weight with
       | None -> ()
       | Some vreg_start ->
         Doubly_linked.insert_after t.links ~after:vreg vreg_start;
         ());
      Option_array.set_some t.heap weight vreg;
      ())
  ;;

  let remove_max t =
    while
      t.min_weight < Option_array.length t.heap
      && Option_array.is_none t.heap t.min_weight
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
         Table.remove t.links vreg_start;
         vreg_start)
  ;;

  let remove t vreg =
    let curr_weight = t.links.!(vreg).weight in
    let vreg_start = Option_array.get_some_exn t.heap curr_weight in
    if Vreg_id.equal vreg_start vreg
    then (
      Option_array.set t.heap curr_weight t.links.!(vreg_start).next;
      ())
    else ();
    Doubly_linked.remove t.links vreg;
    Table.remove t.links vreg;
    ()
  ;;

  let increase t vreg by =
    assert (by >= 0);
    let curr_weight = t.links.!(vreg).weight in
    remove t vreg;
    add_exn t vreg (curr_weight + by);
    ()
  ;;
end

let simplicial_elimination_order interference precolored =
  let heap = Bounded_heap.create ~weight_bound:(Table.length interference) () in
  let id_to_name =
    Table.iter_keys interference
    |> Iter.map ~f:(fun vreg -> vreg.id, vreg)
    |> Id.Table.of_iter
  in
  Table.iter_keys interference
  |> Iter.filter ~f:(fun vreg -> not (Set.mem precolored vreg))
  |> Iter.iter ~f:(fun vreg -> Bounded_heap.add_exn heap vreg.id 0);
  let increase_neighbor_weights vreg =
    Set.iter interference.!(vreg) ~f:(fun neighbor ->
      Bounded_heap.increase heap neighbor.id 1;
      ());
    ()
  in
  Set.iter precolored ~f:increase_neighbor_weights;
  Iter.unfoldr ~init:() ~f:(fun () ->
    let open Option.Let_syntax in
    let%bind vreg_id = Bounded_heap.remove_max heap in
    let vreg = Id.Table.find_exn id_to_name vreg_id in
    increase_neighbor_weights vreg;
    Some (vreg, ()))
;;

let color_graph ~graph ~precolored = todo ()
let alloc_colors ~reg_hashable = todo ()
