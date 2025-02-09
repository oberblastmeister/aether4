open Std
module Entity_table = Ae_entity_table
module Entity_map = Ae_entity_map
module Entity_set = Ae_entity_set

type 'k t = int [@@deriving sexp, compare, equal, hash]

let to_int i = i
let unchecked_of_int i = i
let unchecked_coerce i = i
let succ = succ
let pred = pred

module type Intable = sig
  type t

  val to_int : t -> int
end

module Arg = struct
  type nonrec 'k t = 'k t

  let sexp_of_t = sexp_of_t
  let to_int x = x
end

module Table = Entity_table.Make_phantom (Arg)
module Map = Entity_map.Make_phantom (Arg)
module Set = Entity_set.Make_phantom (Arg)
