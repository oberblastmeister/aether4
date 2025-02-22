open Std
module Entity = Ae_entity_std
module Ident = Entity.Ident
module Graph = Ae_data_graph_std

let get_pred_table ({ succs; all_nodes } : _ Graph.t) =
  let preds = Ident.Table.create () in
  Iter.iter all_nodes ~f:(fun n ->
    succs n
    |> Iter.iter ~f:(fun n' ->
      Ident.Table.update preds n' ~f:(fun o ->
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
