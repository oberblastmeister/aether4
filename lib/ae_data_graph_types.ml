open Std

type 'v t =
  { succs : 'v -> 'v Iter.t
  ; all_nodes : 'v Iter.t
  }
