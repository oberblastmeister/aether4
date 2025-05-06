open Std

type 'v t =
  { succs : 'v -> 'v Iter.t
  ; all_nodes : 'v Iter.t
  }

let of_map_list map =
  { succs = (fun v ~f -> Map.find map v |> (Option.iter @> List.iter) ~f)
  ; all_nodes = Map.iter_keys map
  }
;;

let of_map_set map =
  { succs = (fun v ~f -> Map.find map v |> (Option.iter @> Set.iter) ~f)
  ; all_nodes = Map.iter_keys map
  }
;;

let of_table table =
  { succs = (fun v ~f -> Hashtbl.find table v |> (Option.iter @> List.iter) ~f)
  ; all_nodes = Hashtbl.iter_keys table
  }
;;

module Bi = struct
  type 'v t =
    { succs : 'v -> 'v Iter.t
    ; preds : 'v -> 'v Iter.t
    ; all_nodes : 'v Iter.t
    }

  let to_t { succs; preds = _; all_nodes } = { succs; all_nodes }
end
