open Std

type 'v t =
  { succs : 'v -> 'v Iter.t
  ; all_nodes : 'v Iter.t
  }

module Bi = struct
  type 'v t =
    { succs : 'v -> 'v Iter.t
    ; preds : 'v -> 'v Iter.t
    ; all_nodes : 'v Iter.t
    }

  let to_t { succs; preds = _; all_nodes } = { succs; all_nodes }
end
