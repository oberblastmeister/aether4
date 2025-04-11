open Std
open Ae_trace
module Entity = Ae_entity_std
module Table = Entity.Ident.Table
module Ident = Entity.Ident
module Label_entity = Ae_label_entity
module Label = Label_entity.Ident
module Graph = Ae_data_graph_std
module Dfs = Label_entity.Dfs

type node =
  { idom : Label.t
  ; rpo_number : int
  }
[@@deriving sexp_of]

let compute_idoms ?node_length ~start (graph : Label.t Graph.Bi.t) : node Label.Table.t =
  let idoms = Table.create ?size:node_length () in
  let with_processed label f =
    Table.set idoms ~key:label ~data:{ idom = label; rpo_number = 0 };
    let res = f () in
    Table.remove idoms label;
    res
  in
  (* This is needed so that the start is known to be already processed *)
  (* We will remove this after everything is done *)
  let@ () = with_processed start in
  let is_processed label = Table.mem idoms label in
  (* get the nodes *)
  let nodes = Dfs.reverse_postorder ~start:[ start ] (Graph.Bi.to_t graph) in
  let index_of_node = Table.create ~size:(Vec.length nodes) () in
  Vec.iteri nodes ~f:(fun i node -> Table.set index_of_node ~key:node ~data:i);
  let intersect node1 node2 =
    let with_index node = node, Table.find_exn index_of_node node in
    let go_up node = with_index (Table.find_exn idoms node).idom in
    let rec go (node1, i1) (node2, i2) =
      (* the ones with lower indexes are closer to the top of the rpo tree *)
      match Ordering.of_int (compare i1 i2) with
      | Less -> go (node1, i1) (go_up node2)
      | Greater -> go (go_up node1) (node2, i2)
      | Equal -> node1
    in
    go (with_index node1) (with_index node2)
  in
  let compute () =
    Vec.foldi nodes ~init:false ~f:(fun rpo_number changed node ->
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
          not
          @@ [%equal: Label.t option]
               (Option.map ~f:__.idom (Table.find idoms node))
               (Some new_idom)
        in
        Table.set idoms ~key:node ~data:{ idom = new_idom; rpo_number };
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
    then begin
      (* add node to runner's frontier set because runner doesn't dominate node *)
      Table.update frontier_of_node runner ~f:(function
        | None -> Ident.Set.singleton node
        | Some fs -> Ident.Set.add fs node);
      add_until node node_idom (Table.find_exn idoms runner).idom
    end
  in
  begin
    let@: node =
      graph.all_nodes
      |> Iter.filter ~f:(fun node ->
        (* Table.mem idoms node means that the node is reachable from the start node *)
        graph.preds node |> Iter.length |> is_join_point && Table.mem idoms node)
    in
    let@: pred = graph.preds node in
    add_until node (Table.find_exn idoms node).idom pred;
    ()
  end;
  frontier_of_node
;;

type t =
  { start : Label.t
  ; table : node Label.Table.t
  }
[@@deriving sexp_of]

let find t l = Option.map ~f:__.idom (Table.find t.table l)

let dominates t ~higher ~lower =
  match Table.find t.table higher, Table.find t.table lower with
  | None, _ | _, None -> false
  | Some higher_node, Some _lower_node ->
    let rec loop lower =
      if higher_node.rpo_number < (Table.find_exn t.table lower).rpo_number
      then begin
        let idom = Table.find t.table lower in
        match idom with
        | Some idom -> loop idom.idom
        | None ->
          (* we climbed past the entry *)
          false
      end
      else Label.equal higher lower
    in
    loop lower
;;

let is_reachable t l = Label.equal t.start l || Table.mem t.table l

let compute ?node_length ~start graph =
  let table = compute_idoms ?node_length ~start graph in
  { start; table }
;;

module Tree = struct
  type t = Label.t list Label.Table.t [@@deriving sexp_of]

  let children idoms label = Table.find idoms label |> Option.value ~default:[]

  let of_immediate idoms =
    Table.iteri idoms.table
    |> Iter.map ~f:(fun (label, node) -> node.idom, label)
    |> Table.of_iter_accum ~init:[] ~f:(fun acc label -> label :: acc)
  ;;
end

module Frontier = struct
  type t = Label.Set.t Label.Table.t [@@deriving sexp_of]

  let compute idoms = compute_frontier idoms.table

  let find (t : t) label =
    Table.find t label |> Option.value_map ~f:Ident.Set.to_list ~default:[]
  ;;

  let find_iter (t : t) label =
    Table.find t label |> Option.value_map ~f:Ident.Set.iter ~default:Iter.empty
  ;;
end
