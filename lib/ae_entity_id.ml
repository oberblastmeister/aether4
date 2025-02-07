open Std
module Entity_map = Ae_entity_map

type 'k t = int [@@deriving sexp, compare, equal, hash]

let unchecked_of_int i = i
let unchecked_coerce i = i

module type Intable = sig
  type t

  val to_int : t -> int
end

module Table = Entity_map.Make (struct
    type nonrec 'k t = 'k t

    let sexp_of_t = sexp_of_t
    let to_int x = x
  end)

module Map = struct
  type ('w, 'a) t = 'a Int.Map.t

  let empty = Int.Map.empty
  let singleton = Int.Map.singleton
end
