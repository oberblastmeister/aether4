open Std
module Entity = Ae_entity_std
module Temp_entity = Ae_abs_asm_temp_entity
module Temp = Ae_abs_asm_temp_entity.Ident
module Temp_id = Temp_entity.Id
module Id = Entity.Id
module Table = Entity.Ident.Table
module Ident = Entity.Ident
module Bounded_heap = Ae_bounded_heap
open Ae_trace
open Table.Syntax

module Graph = struct
  type t = Temp.Set.t Temp.Table.t [@@deriving sexp_of]

  let create () = Table.create ()
  let add (t : t) v = Table.find_or_add t v ~default:(fun () -> Ident.Set.empty) |> ignore
  let mem (t : t) v = Table.mem t v
  let iter_temp (t : t) ~f = Table.iter_keys t ~f

  let add_edge (t : t) v1 v2 =
    Table.update t v1 ~f:(function
      | None -> Ident.Set.singleton v2
      | Some set -> Ident.Set.add set v2);
    Table.update t v2 ~f:(function
      | None -> Ident.Set.singleton v1
      | Some set -> Ident.Set.add set v1);
    ()
  ;;
end

let simplicial_elimination_order (graph : Graph.t) (precolored : int Temp.Map.t) ~f =
  let heap = Bounded_heap.create ~weight_bound:(Table.length graph) () in
  let id_to_name =
    Table.iter_keys graph |> Iter.map ~f:(fun temp -> temp.id, temp) |> Id.Table.of_iter
  in
  Table.iter_keys graph
  |> Iter.filter ~f:(fun temp -> not (Ident.Map.mem precolored temp))
  |> Iter.iter ~f:(fun (temp : Temp.t) -> Bounded_heap.add_exn heap temp.id 0);
  let increase_neighbor_weights temp =
    Ident.Set.iter graph.!(temp) ~f:(fun neighbor ->
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
  |> Iter.iter ~f
;;

let color_graph_with_ordering
      ~(spilled_color : int)
      ~available_colors
      ~ordering
      ~precolored
      ~(graph : Graph.t)
  =
  let temp_to_color : int Temp.Table.t = Entity.Ident.Table.create () in
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
  temp_to_color, !used_colors
;;

let color_graph ~spilled_color ~available_colors ~graph ~precolored =
  color_graph_with_ordering
    ~spilled_color
    ~available_colors
    ~ordering:(simplicial_elimination_order graph precolored)
    ~precolored
    ~graph
;;
