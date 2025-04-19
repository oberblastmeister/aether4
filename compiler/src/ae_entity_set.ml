open Std

module Entry = struct
  type ('k, 'v) t =
    { key : 'k
    ; data : 'v
    }
  [@@deriving sexp_of, compare, equal]
end

module type Key_phantom = sig
  type 'w t [@@deriving sexp_of]

  val to_int : 'w t -> int
end

module type Key = sig
  type t

  include Key_phantom with type 'w t := t
end

module type S_phantom_without_make = sig
  module Key : Key_phantom

  type 'w t

  val empty : 'w t
  val length : 'w t -> int
  val union : 'w t -> 'w t -> 'w t
  val singleton : 'w Key.t -> 'w t
  val remove : 'w t -> 'w Key.t -> 'w t
  val add : 'w t -> 'w Key.t -> 'w t
  val mem : 'w t -> 'w Key.t -> bool
  val iter : 'w t -> 'w Key.t Iter.t
  val fold : 'w t -> init:'a -> f:('a -> 'w Key.t -> 'a) -> 'a
  val to_list : 'w t -> 'w Key.t list
  val of_list_exn : 'w Key.t list -> 'w t
end

module type S_phantom = sig
  include S_phantom_without_make

  module Make (Witness : Ae_entity_witness.S) : sig
    type 'w t' := 'w t
    type t = Witness.t t' [@@deriving sexp_of]
  end
end

module type S = sig
  type t

  include S_phantom_without_make with type 'w t := t
end

module Make_phantom (Key : Key_phantom) : S_phantom with module Key = Key = struct
  type 'w t = 'w Key.t Int.Map.t

  module Key = Key

  let union = Map.merge_skewed ~combine:(fun ~key:_ e1 _e2 -> e1)
  let empty = Int.Map.empty
  let length = Map.length
  let remove t k = Map.remove t (Key.to_int k)
  let singleton k = Int.Map.singleton (Key.to_int k) k
  let to_list m = Map.to_alist m |> List.map ~f:snd
  let add t k = Map.set t ~key:(Key.to_int k) ~data:k
  let mem t k = Map.mem t (Key.to_int k)
  let iter t ~f = Map.iter t ~f
  let fold t ~init ~f = Map.fold t ~init ~f:(fun ~key:_ ~data acc -> f acc data)
  let of_list_exn l = List.map ~f:(fun k -> Key.to_int k, k) l |> Int.Map.of_alist_exn
  let sexp_of_t _ t = to_list t |> List.sexp_of_t (Key.sexp_of_t sexp_of_opaque)

  module Make (Witness : Ae_entity_witness.S) = struct
    open struct
      type 'w t' = 'w t [@@deriving sexp_of]
    end

    type t = Witness.t t' [@@deriving sexp_of]
  end
end

module Make (Key : Key) : S = struct
  module Key' = struct
    include Key

    type 'w t = Key.t
  end

  include Make_phantom (Key')

  type nonrec t = Nothing.t t
end
