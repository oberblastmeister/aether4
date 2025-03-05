open Std
module Entity = Ae_entity_std
module Table = Entity.Ident.Table
module Ident = Entity.Ident
module Label_entity = Ae_label_entity
module Label = Label_entity.Ident
module Graph = Ae_data_graph_std
module Dfs = Label_entity.Dfs

let compute_idoms ?node_length ~start (graph : Label.t Graph.Bi.t) =
  let idoms = Table.create ?size:node_length () in
  let with_processed label f =
    Table.set idoms ~key:label ~data:label;
    let res = f () in
    Table.remove idoms label;
    res
  in
  let is_processed label = Table.mem idoms label in
  let@ () = with_processed start in
  (* This is needed so that the start is known to be already processed *)
  (* We will remove this after everything is done *)
  Table.set idoms ~key:start ~data:start;
  Table.set idoms ~key:start ~data:start;
  (* get the nodes *)
  let nodes = Dfs.postorder ~start:[ start ] (Graph.Bi.to_t graph) in
  let index_of_node = Table.create ~size:(Vec.length nodes) () in
  (* since we iterate in postorder, things higher up the REVERSE POST ORDER tree will have a higher index *)
  Vec.iteri nodes ~f:(fun i node -> Table.set index_of_node ~key:node ~data:i);
  let intersect node1 node2 =
    let with_index node = node, Table.find_exn index_of_node node in
    let go_up node = with_index (Table.find_exn idoms node) in
    let rec go (node1, i1) (node2, i2) =
      (* the ones with higher indexes are closer to the top of the rpo tree *)
      match Ordering.of_int ([%compare: int] i1 i2) with
      | Less -> go (go_up node1) (node2, i2)
      | Greater -> go (node1, i1) (go_up node2)
      | Equal -> node1
    in
    go (with_index node1) (with_index node2)
  in
  let compute () =
    (* fold from the right for REVERSE POST ORDER *)
    Vec.fold_right nodes ~init:false ~f:(fun node changed ->
      if [%equal: Label.t] start node
      then changed
      else (
        let preds = Iter.to_list (graph.preds node) in
        (* set the fake idom to the first processed idom *)
        let idom_approx =
          preds
          |> List.find ~f:is_processed
          |> Option.value_exn
               ~message:
                 "must have idom because reverse post order guarantees that we have \
                  processed at least one predecessor"
        in
        let other_preds =
          preds |> List.filter ~f:(fun pred -> not @@ [%equal: Label.t] pred idom_approx)
        in
        let new_idom =
          List.iter other_preds
          |> Iter.filter ~f:is_processed
          |> Iter.fold ~init:idom_approx ~f:(fun current_idom pred ->
            intersect pred current_idom)
        in
        let changed =
          not @@ [%equal: Label.t option] (Table.find idoms node) (Some new_idom)
        in
        Table.set idoms ~key:node ~data:new_idom;
        changed))
  in
  let rec fixpoint () =
    match compute () with
    | true -> fixpoint ()
    | false -> ()
  in
  fixpoint ();
  idoms
;;

let compute_frontier idoms (graph : Label.t Graph.Bi.t) =
  let is_join_point preds_length = preds_length >= 2 in
  let frontier_of_node = Table.create () in
  let rec add_until node node_idom runner =
    if not @@ Label.equal runner node_idom
    then (
      (* add node to runner's frontier set because runner doesn't dominate node *)
      Table.update frontier_of_node runner ~f:(function
        | None -> Ident.Set.singleton node
        | Some fs -> Ident.Set.add fs node);
      add_until node node_idom (Table.find_exn idoms runner);
      ())
  in
  graph.all_nodes
  |> Iter.iter ~f:(fun node ->
    let num_preds = graph.preds node |> Iter.length in
    if is_join_point num_preds
    then
      Iter.iter (graph.preds node) ~f:(fun pred ->
        (* every node must have an idom *)
        add_until node (Table.find_exn idoms node) pred));
  frontier_of_node
;;

module Immediate = struct
  type t = Label.t Label.Table.t [@@deriving sexp_of]

  let find = Table.find
  let compute = compute_idoms
end

module Tree = struct
  type t = Label.t list Label.Table.t [@@deriving sexp_of]

  let children idoms label = Table.find idoms label |> Option.value ~default:[]

  let of_immediate idoms =
    Table.iteri idoms
    |> Iter.map ~f:(fun (label, idom) -> idom, label)
    |> Table.of_iter_accum ~init:[] ~f:(fun acc label -> label :: acc)
  ;;
end

module Frontier = struct
  type t = Label.Set.t Label.Table.t [@@deriving sexp_of]

  let compute = compute_frontier

  let find (t : t) label =
    Table.find t label |> Option.value_map ~f:Ident.Set.to_list ~default:[]
  ;;

  let find_iter (t : t) label =
    Table.find t label |> Option.value_map ~f:Ident.Set.iter ~default:Iter.empty
  ;;
end
