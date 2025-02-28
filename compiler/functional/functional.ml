module Fold = Functional_fold
module Iter = Functional_iter
(* conflicts with stdlib Map *)
(* module Map = Functional_map *)

module Syntax = struct
  let ( let@ ) f x = f x
  let ( let@: ) f x = f ~f:x
  let ( @> ) = Fold.( @> )
  let ( & ) = Functional_map.( & )
  let for_ = Iter.for_
end
