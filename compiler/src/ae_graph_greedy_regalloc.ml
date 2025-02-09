open Std
module Entity = Ae_entity_std
module Vreg_entity = Ae_vreg_entity
module Vreg = Ae_vreg_entity.Name
module Vreg_id = Vreg_entity.Id
module Id = Entity.Id
module Table = Entity.Name.Table
module Set = Entity.Name.Set
module Bounded_heap = Ae_bounded_heap
open Table.Syntax

module Graph = struct
  type t = Vreg.Set.t Vreg.Table.t

  let create () = Table.create ()
  let add (t : t) v = Table.find_or_add t v ~default:(fun () -> Set.empty) |> ignore

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

let simplicial_elimination_order (graph : Graph.t) (precolored : Vreg.Set.t) ~f =
  let heap = Bounded_heap.create ~weight_bound:(Table.length graph) () in
  let id_to_name =
    Table.iter_keys graph |> Iter.map ~f:(fun vreg -> vreg.id, vreg) |> Id.Table.of_iter
  in
  Table.iter_keys graph
  |> Iter.filter ~f:(fun vreg -> not (Set.mem precolored vreg))
  |> Iter.iter ~f:(fun (vreg : Vreg.t) -> Bounded_heap.add_exn heap vreg.id 0);
  let increase_neighbor_weights vreg =
    Set.iter graph.!(vreg) ~f:(fun neighbor ->
      if Bounded_heap.mem heap neighbor.id
      then Bounded_heap.increase_exn heap neighbor.id 1;
      ());
    ()
  in
  Set.iter precolored ~f:increase_neighbor_weights;
  Iter.unfoldr ~init:() ~f:(fun () ->
    let open Option.Let_syntax in
    let%bind vreg_id, _ = Bounded_heap.remove_max heap in
    let vreg = Id.Table.find_exn id_to_name vreg_id in
    increase_neighbor_weights vreg;
    Some (vreg, ()))
  |> Iter.iter ~f
;;

(* precondition: colors should be sorted *)
let find_gaps (colors : Color.t Array.t) : Color.t =
  Array.iteri colors
  |> Iter.uncurry
  |> Iter.map ~f:(fun (i, c) ->
    if i = 0 then Id.unchecked_of_int (-1), c else colors.(i - 1), c)
  |> (fun i ->
  if Array.is_empty colors
  then i
  else Iter.snoc i (Array.last colors, Id.succ (Id.succ (Array.last colors))))
  |> Iter.find_map ~f:(fun (c, c') ->
    if Id.to_int (Id.succ c) < Id.to_int c' then Some (Id.succ c) else None)
  |> Option.value ~default:(Id.unchecked_of_int 0)
;;

let%expect_test _ =
  let id i : Color.t = Id.unchecked_of_int i in
  let check cs = print_s [%sexp (find_gaps cs : Color.t)] in
  check [| id 0; id 1; id 1; id 2; id 4; id 5 |];
  [%expect {| 3 |}];
  check [| id 1; id 3; id 4 |];
  [%expect {| 0 |}];
  check [| id 0; id 1; id 2; id 3 |];
  [%expect {| 4 |}];
  check [| id 0; id 1; id 5; id 6 |];
  [%expect {| 2 |}];
  check [| id 0 |];
  [%expect {| 1 |}];
  ()
;;

let color_graph_with_ordering ordering (graph : Graph.t) (precolored : Vreg.Set.t) =
  let vreg_to_color : Color.t Vreg.Table.t = Entity.Name.Table.create () in
  let max_color : Color.t ref = ref @@ Id.unchecked_of_int (-1) in
  (* color the precolored vregs *)
  (* each precolored vreg conflicts with all other precolored vregs *)
  Set.iter precolored ~f:(fun (vreg : Vreg.t) ->
    max_color := Id.succ !max_color;
    Table.set vreg_to_color ~key:vreg ~data:!max_color;
    ());
  ordering
  |> Iter.iter ~f:(fun vreg ->
    let neighbor_colors =
      Set.iter graph.!(vreg)
      |> Iter.filter_map ~f:(Table.find vreg_to_color)
      |> Iter.to_array
    in
    Array.sort neighbor_colors ~compare:Color.compare;
    let lowest_not_in_neighbors = find_gaps neighbor_colors in
    vreg_to_color.!(vreg) <- lowest_not_in_neighbors;
    max_color
    := Id.unchecked_of_int
         (max (Id.to_int !max_color) (Id.to_int lowest_not_in_neighbors));
    ());
  vreg_to_color
;;

let color_graph graph precolored =
  color_graph_with_ordering
    (simplicial_elimination_order graph precolored)
    graph
    precolored
;;
