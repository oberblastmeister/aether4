module Fold = Functional_fold
module Iter = Functional_iter
(* conflicts with stdlib Map *)
(* module Map = Functional_map *)

module Syntax = struct
  let[@inline] ( let@ ) f x = f x
  let[@inline] ( let@: ) f x = f ~f:x
  let ( @> ) = Fold.( @> )
  let ( & ) = Functional_map.( & )
end
