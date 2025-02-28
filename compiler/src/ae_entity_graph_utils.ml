open Std
module Entity = Ae_entity_std
module Ident = Entity.Ident
module Graph = Ae_data_graph_std

let get_pred_table ({ succs; all_nodes } : _ Graph.t) =
  let open Ident.Table.Syntax in
  let preds = Ident.Table.create () in
  Iter.iter all_nodes ~f:(fun n ->
    (* make sure to initialize to empty *)
    if not (Ident.Table.mem preds n) then preds.!(n) <- [];
    succs n
    |> Iter.iter ~f:(fun succ ->
      Ident.Table.update preds succ ~f:(fun o ->
        Option.value_map ~default:[ n ] ~f:(List.cons n) o)));
  preds
;;

let graph_of_adj_map map =
  Graph.
    { succs =
        (fun n ->
          Ident.Map.find map n |> Option.value_map ~default:Iter.empty ~f:List.iter)
    ; all_nodes = Ident.Map.iter_keys map
    }
;;

let graph_of_adj_table table =
  Graph.
    { succs =
        (fun n ->
          Ident.Table.find table n |> Option.value_map ~default:Iter.empty ~f:List.iter)
    ; all_nodes = Ident.Table.iter_keys table
    }
;;

let bi_graph_of_adj_table ~succ_table ~pred_table =
  Graph.Bi.
    { succs =
        (fun n ->
          Ident.Table.find succ_table n
          |> Option.value_map ~default:Iter.empty ~f:List.iter)
    ; preds =
        (fun n ->
          Ident.Table.find pred_table n
          |> Option.value_map ~default:Iter.empty ~f:List.iter)
    ; all_nodes = Ident.Table.iter_keys succ_table
    }
;;

let to_bi g =
  let pred_table = get_pred_table g in
  Graph.Bi.
    { succs = g.succs
    ; preds =
        (fun n ->
          Ident.Table.find pred_table n
          |> Option.value_map ~default:Iter.empty ~f:List.iter)
    ; all_nodes = g.all_nodes
    }
;;
