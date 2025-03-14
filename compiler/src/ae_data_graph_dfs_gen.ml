open Std
open Ae_data_graph_types
module Signatures = Ae_signatures

type 'n event =
  | Cycle of 'n
  | Enter of 'n
  | Exit of 'n

module Make (Set : Signatures.Mutable_set) = struct
  let visit ~f ~start (graph : _ t) =
    let visited = Set.create () in
    let rec go node =
      Set.add visited node;
      f (Enter node);
      graph.succs node ~f:(fun node ->
        if Set.mem visited node then f (Cycle node) else go node);
      f (Exit node)
    in
    List.iter ~f:go start
  ;;

  let visit_preorder ~f =
    visit ~f:(function
      | Enter node -> f node
      | _ -> ())
  ;;

  let visit_postorder ~f =
    visit ~f:(function
      | Exit node -> f node
      | _ -> ())
  ;;

  let postorder ~start graph =
    let vec = Vec.create () in
    visit_postorder ~f:(Vec.push_back vec) ~start graph;
    vec
  ;;

  let reverse_postorder ~start graph =
    let vec = postorder ~start graph in
    Vec.to_list vec |> List.rev |> Vec.of_list
  ;;

  let preorder ~start graph =
    let vec = Vec.create () in
    visit_preorder ~f:(Vec.push_back vec) ~start graph;
    vec
  ;;
end
