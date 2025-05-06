open Std
open Ae_data_graph_types
module Signatures = Ae_signatures

module Make
    (Map : Signatures.Mutable_map)
    (Set : Signatures.Mutable_set with module Key = Map.Key) =
struct
  let visit ~start (graph : _ t) ~f =
    let visited = Set.create () in
    let stack = Stack.create () in
    List.iter start ~f:(fun node -> Stack.push stack (`Enter node));
    while not (Stack.is_empty stack) do
      let event = Stack.pop_exn stack in
      match event with
      | `Enter node when Set.mem visited node -> f (`Already_visited node)
      | `Enter node ->
        Stack.push stack (`Exit node);
        Set.add visited node;
        f (`Enter node);
        graph.succs node ~f:(fun node -> Stack.push stack (`Enter node))
      | `Exit node -> f (`Exit node)
    done
  ;;

  let visit_and_classify ~start (graph : _ t) ~f =
    let visited = Set.create () in
    let visiting = Set.create () in
    let stack = Stack.create () in
    begin
      let@: node = start in
      Stack.push stack (`Enter (node, [ node ]));
      while not (Stack.is_empty stack) do
        let event = Stack.pop_exn stack in
        match event with
        | `Enter (v, path) ->
          if not (Set.mem visited v)
          then begin
            Stack.push stack (`Exit v);
            Set.add visited v;
            Set.add visiting v;
            graph.succs v ~f:(fun w -> Stack.push stack (`Edge (v, w, w :: path)))
          end
        | `Exit v ->
          Set.remove visiting v;
          f (`Exit v)
        | `Edge (v, w, path) ->
          let kind =
            if Set.mem visited w
            then begin
              if Set.mem visiting w then `Back else `Cross
            end
            else begin
              Stack.push stack (`Enter (w, path));
              `Tree
            end
          in
          f (`Edge (v, w, path, kind))
      done
    end
  ;;

  let visit_preorder ~f =
    visit ~f:(function
      | `Enter node -> f node
      | _ -> ())
  ;;

  let visit_postorder ~f =
    visit ~f:(function
      | `Exit node -> f node
      | _ -> ())
  ;;

  let postorder ~start graph =
    let vec = Vec.create () in
    visit_postorder ~f:(Vec.push_back vec) ~start graph;
    vec
  ;;

  let reverse_postorder ~start graph =
    let vec = postorder ~start graph in
    let array = Vec.to_array vec in
    Array.rev_inplace array;
    Vec.of_array array
  ;;

  let preorder ~start graph =
    let vec = Vec.create () in
    visit_preorder ~f:(Vec.push_back vec) ~start graph;
    vec
  ;;

  let find_cycle ~equal graph =
    let open Option.Let_syntax in
    let%bind v, w, path =
      visit_and_classify ~start:graph.all_nodes graph
      |> Iter.find_map ~f:(function
        | `Edge (v, w, path, `Back) -> Some (v, w, path)
        | _ -> None)
    in
    let path = List.rev path |> List.drop_while ~f:(fun node -> not (equal node w)) in
    return (v, w, path)
  ;;
end
