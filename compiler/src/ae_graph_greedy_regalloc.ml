(* TODO: fix this file *)
open Std
module Bounded_heap = Ae_bounded_heap
module Temp = Ae_temp
open Ae_trace

module Graph = struct
  type t = Temp.Set.t Temp.Table.t [@@deriving sexp_of]

  let create () = Temp.Table.create ()

  let add (t : t) v =
    Temp.Table.find_or_add t v ~default:(fun () -> Temp.Set.empty) |> ignore
  ;;

  let mem (t : t) v = Temp.Table.mem t v
  let iter_temp (t : t) ~f = Temp.Table.iter_keys t ~f

  let add_edge (t : t) v1 v2 =
    Temp.Table.update t v1 ~f:(function
      | None -> Temp.Set.singleton v2
      | Some set -> Set.add set v2);
    Temp.Table.update t v2 ~f:(function
      | None -> Temp.Set.singleton v1
      | Some set -> Set.add set v1);
    ()
  ;;
end

let simplicial_elimination_order (graph : Graph.t) (precolored : int Temp.Map.t) ~f =
  (* let heap = Bounded_heap.create ~weight_bound:(Temp.Table.length graph) () in
  let id_to_name =
    Temp.Table.iter_keys graph |> Iter.map ~f:(fun temp -> temp.id, temp) |> Id.Table.of_iter
  in
  Temp.Table.iter_keys graph
  |> Iter.filter ~f:(fun temp -> not (Map.mem precolored temp))
  |> Iter.iter ~f:(fun (temp : Temp.t) -> Bounded_heap.add_exn heap temp.id 0);
  let increase_neighbor_weights temp =
    Set.iter graph.Temp.Table.Syntax.!(temp) ~f:(fun neighbor ->
      if Bounded_heap.mem heap neighbor.id
      then Bounded_heap.increase_exn heap neighbor.id 1;
      ());
    ()
  in
  Ident.Map.iter_keys precolored
  |> Iter.filter ~f:(Graph.mem graph)
  |> Iter.iter ~f:increase_neighbor_weights;
  Iter.unfoldr ~init:() ~f:(fun () ->
    let open Option.Let_syntax in
    let%bind temp_id, _ = Bounded_heap.remove_max heap in
    let temp = Id.Table.find_exn id_to_name temp_id in
    increase_neighbor_weights temp;
    Some (temp, ()))
  |> Iter.iter ~f *)
  todol [%here]
;;

let color_graph_with_ordering
      ~(spilled_color : int)
      ~available_colors
      ~ordering
      ~precolored
      ~(graph : Graph.t)
  =
  (* let temp_to_color : int Temp.Table.t = Entity.Ident.Table.create () in
  let used_colors = ref available_colors in
  ordering
  |> Iter.iter ~f:(fun temp ->
    let neighbor_colors =
      Ident.Set.iter graph.!(temp)
      |> Iter.filter_map ~f:(fun temp ->
        match Ident.Map.find precolored temp with
        | Some color ->
          assert (not (Table.mem temp_to_color temp));
          Some color
        | None -> Table.find temp_to_color temp)
      |> Iter.to_list
      |> Int.Set.of_list
    in
    let color =
      Set.diff available_colors neighbor_colors
      |> Set.min_elt
      |> Option.value_or_thunk ~default:(fun () ->
        let color = ref spilled_color in
        begin
          let@ r = with_return in
          while true do
            if not (Set.mem neighbor_colors !color) then r.return ();
            incr color
          done
        end;
        used_colors := Set.add !used_colors !color;
        !color)
    in
    temp_to_color.!(temp) <- color;
    ());
  temp_to_color, !used_colors *)
  todol [%here]
;;

let color_graph ~spilled_color ~available_colors ~graph ~precolored =
  color_graph_with_ordering
    ~spilled_color
    ~available_colors
    ~ordering:(simplicial_elimination_order graph precolored)
    ~precolored
    ~graph
;;
