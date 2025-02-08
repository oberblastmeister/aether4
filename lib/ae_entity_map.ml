open Std

module Entry = struct
  type ('k, 'v) t =
    { key : 'k
    ; data : 'v
    }
  [@@deriving sexp_of, compare, equal]
end

module type Key = sig
  type 'w t [@@deriving sexp_of]

  val to_int : 'w t -> int
end

module type Key0 = sig
  type t

  include Key with type 'w t := t
end

module type S_without_make = sig
  module Key : Key

  type ('w, 'a) t

  val empty : ('w, 'a) t
  val singleton : 'w Key.t -> 'a -> ('w, 'a) t
  val find : ('w, 'a) t -> 'w Key.t -> 'a option
  val set : ('w, 'a) t -> key:'w Key.t -> data:'a -> ('w, 'a) t
  val mem : ('w, 'a) t -> 'w Key.t -> bool
  val map : ('w, 'a) t -> f:('a -> 'b) -> ('w, 'b) t
end

module type S = sig
  include S_without_make

  module Make (Witness : Ae_entity_witness.S) : sig
    type ('w, 'v) t' := ('w, 'v) t
    type 'v t = (Witness.t, 'v) t' [@@deriving sexp_of]
  end
end

module type S0 = sig
  type 'a t

  include S_without_make with type ('w, 'a) t := 'a t
end

module Make (Key : Key) : S with module Key = Key = struct
  type ('w, 'v) t = ('w Key.t, 'v) Entry.t Int.Map.t [@@deriving sexp_of]

  module Key = Key

  let empty = Int.Map.empty
  let singleton key data = Int.Map.singleton (Key.to_int key) { Entry.key; data }
  let find t k = Map.find t (Key.to_int k) |> Option.map ~f:(fun e -> e.Entry.data)
  let find_exn t k = (Map.find_exn t (Key.to_int k)).Entry.data
  let map t ~f = Map.map t ~f:(fun e -> { e with Entry.data = f e.Entry.data })
  let set t ~key ~data = Map.set t ~key:(Key.to_int key) ~data:{ Entry.key; data }
  let mem t k = Map.mem t (Key.to_int k)

  module Make (Witness : Ae_entity_witness.S) = struct
    open struct
      type ('w, 'v) t' = ('w, 'v) t [@@deriving sexp_of]
    end

    type 'v t = (Witness.t, 'v) t' [@@deriving sexp_of]
  end
end

module Make0 (Key : Key0) : S0 = struct
  module Key' = struct
    include Key

    type 'w t = Key.t
  end

  include Make (Key')

  type nonrec 'a t = (Nothing.t, 'a) t
end
