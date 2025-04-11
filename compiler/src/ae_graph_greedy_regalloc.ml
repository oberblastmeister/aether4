open Std
module Entity = Ae_entity_std
module Temp_entity = Ae_abs_asm_temp_entity
module Temp = Ae_abs_asm_temp_entity.Ident
module Temp_id = Temp_entity.Id
module Id = Entity.Id
module Table = Entity.Ident.Table
module Set = Entity.Ident.Set
module Bounded_heap = Ae_bounded_heap
open Table.Syntax

module Graph = struct
  type t = Temp.Set.t Temp.Table.t [@@deriving sexp_of]

  let create () = Table.create ()
  let add (t : t) v = Table.find_or_add t v ~default:(fun () -> Set.empty) |> ignore
  let mem (t : t) v = Table.mem t v
  let iter_temp (t : t) ~f = Table.iter_keys t ~f

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

let simplicial_elimination_order (graph : Graph.t) (precolored : Temp.Set.t) ~f =
  let heap = Bounded_heap.create ~weight_bound:(Table.length graph) () in
  let id_to_name =
    Table.iter_keys graph |> Iter.map ~f:(fun temp -> temp.id, temp) |> Id.Table.of_iter
  in
  Table.iter_keys graph
  |> Iter.filter ~f:(fun temp -> not (Set.mem precolored temp))
  |> Iter.iter ~f:(fun (temp : Temp.t) -> Bounded_heap.add_exn heap temp.id 0);
  let increase_neighbor_weights temp =
    Set.iter graph.!(temp) ~f:(fun neighbor ->
      if Bounded_heap.mem heap neighbor.id
      then Bounded_heap.increase_exn heap neighbor.id 1;
      ());
    ()
  in
  Set.iter precolored ~f:increase_neighbor_weights;
  Iter.unfoldr ~init:() ~f:(fun () ->
    let open Option.Let_syntax in
    let%bind temp_id, _ = Bounded_heap.remove_max heap in
    let temp = Id.Table.find_exn id_to_name temp_id in
    increase_neighbor_weights temp;
    Some (temp, ()))
  |> Iter.iter ~f
;;

(* precondition: colors should be sorted *)
let find_gap (colors : int Array.t) : int =
  Array.iteri colors
  |> Iter.uncurry
  |> Iter.map ~f:(fun (i, c) -> if i = 0 then -1, c else colors.(i - 1), c)
  |> (fun i ->
  if Array.is_empty colors
  then i
  else Iter.snoc i (Array.last colors, succ (succ (Array.last colors))))
  |> Iter.find_map ~f:(fun (c, c') -> if succ c < c' then Some (succ c) else None)
  |> Option.value ~default:0
;;

let%expect_test _ =
  let check cs = print_s [%sexp (find_gap cs : int)] in
  check [| 0; 1; 1; 2; 4; 5 |];
  [%expect {| 3 |}];
  check [| 1; 3; 4 |];
  [%expect {| 0 |}];
  check [| 0; 1; 2; 3 |];
  [%expect {| 4 |}];
  check [| 0; 1; 5; 6 |];
  [%expect {| 2 |}];
  check [| 0 |];
  [%expect {| 1 |}];
  ()
;;

let color_graph_with_ordering ordering (graph : Graph.t) (precolored : Temp.Set.t) =
  let temp_to_color : int Temp.Table.t = Entity.Ident.Table.create () in
  let max_color : int ref = ref (-1) in
  (* color the precolored temps *)
  (* each precolored temp conflicts with all other precolored temps *)
  Set.iter precolored ~f:(fun (temp : Temp.t) ->
    max_color := succ !max_color;
    Table.set temp_to_color ~key:temp ~data:!max_color;
    ());
  ordering
  |> Iter.iter ~f:(fun temp ->
    let neighbor_colors =
      Set.iter graph.!(temp)
      |> Iter.filter_map ~f:(Table.find temp_to_color)
      |> Iter.to_array
    in
    Array.sort neighbor_colors ~compare;
    let lowest_not_in_neighbors = find_gap neighbor_colors in
    temp_to_color.!(temp) <- lowest_not_in_neighbors;
    max_color := max !max_color lowest_not_in_neighbors;
    ());
  temp_to_color, !max_color
;;

let color_graph graph precolored =
  color_graph_with_ordering
    (simplicial_elimination_order graph precolored)
    graph
    precolored
;;
