open Std

module Entry = struct
  type ('k, 'v) t =
    { key : 'k
    ; data : 'v
    }
  [@@deriving sexp_of, compare, equal]
end

open Ae_entity_sigs

module type S_phantom_without_make = sig
  module Key : Key_phantom

  type ('w, 'a) t

  val empty : ('w, 'a) t
  val singleton : 'w Key.t -> 'a -> ('w, 'a) t
  val find : ('w, 'a) t -> 'w Key.t -> 'a option
  val find_exn : ('w, 'a) t -> 'w Key.t -> 'a
  val add_exn : ('w, 'a) t -> key:'w Key.t -> data:'a -> ('w, 'a) t
  val set : ('w, 'a) t -> key:'w Key.t -> data:'a -> ('w, 'a) t
  val mem : ('w, 'a) t -> 'w Key.t -> bool
  val map : ('w, 'a) t -> f:('a -> 'b) -> ('w, 'b) t
  val iter : ('w, 'a) t -> 'a Iter.t
  val iter_keys : ('w, 'a) t -> 'w Key.t Iter.t
  val to_alist : ('w, 'a) t -> ('w Key.t * 'a) list
  val of_alist_exn : ('w Key.t * 'a) list -> ('w, 'a) t

  module Syntax : sig
    val ( .%() ) : ('w, 'a) t -> 'w Key.t -> 'a
    val ( .%()<- ) : ('w, 'a) t -> 'w Key.t -> 'a -> ('w, 'a) t
  end
end

module type S_phantom = sig
  include S_phantom_without_make

  module Make (Witness : Ae_entity_witness.S) : sig
    type ('w, 'v) t' := ('w, 'v) t
    type 'v t = (Witness.t, 'v) t' [@@deriving sexp_of]
  end
end

module type S = sig
  type 'a t

  include S_phantom_without_make with type ('w, 'a) t := 'a t
end

module Make_phantom (Key : Key_phantom) : S_phantom with module Key = Key = struct
  type ('w, 'v) t = ('w Key.t, 'v) Entry.t Int.Map.t

  module Key = Key

  let empty = Int.Map.empty
  let singleton key data = Int.Map.singleton (Key.to_int key) { Entry.key; data }
  let find t k = Map.find t (Key.to_int k) |> Option.map ~f:(fun e -> e.Entry.data)
  let find_exn t k = (Map.find_exn t (Key.to_int k)).Entry.data
  let map t ~f = Map.map t ~f:(fun e -> { e with Entry.data = f e.Entry.data })
  let set t ~key ~data = Map.set t ~key:(Key.to_int key) ~data:{ Entry.key; data }
  let to_alist t = Map.to_alist t |> List.map ~f:(fun (_, e) -> e.Entry.key, e.data)
  let mem t k = Map.mem t (Key.to_int k)
  let add_exn t ~key ~data = Map.add_exn t ~key:(Key.to_int key) ~data:{ Entry.key; data }
  let iter t ~f = Map.iter t ~f:(fun e -> f e.Entry.data)
  let iter_keys t ~f = Map.iter t ~f:(fun e -> f e.Entry.key)

  let of_alist_exn l =
    List.map l ~f:(fun (key, data) -> Key.to_int key, Entry.{ key; data })
    |> Int.Map.of_alist_exn
  ;;

  let sexp_of_t _ f t =
    to_alist t |> List.sexp_of_t (Tuple2.sexp_of_t (Key.sexp_of_t sexp_of_opaque) f)
  ;;

  module Syntax = struct
    let ( .%() ) = find_exn
    let ( .%()<- ) t key data = set t ~key ~data
  end

  module Make (Witness : Ae_entity_witness.S) = struct
    open struct
      type ('w, 'v) t' = ('w, 'v) t [@@deriving sexp_of]
    end

    type 'v t = (Witness.t, 'v) t' [@@deriving sexp_of]
  end
end

module Make (Key : Key) : S with type 'w Key.t = Key.t = struct
  module Key' = struct
    type 'w t = Key.t

    let to_int k = Key.to_int k
    let sexp_of_t _ k = Key.sexp_of_t k
  end

  include Make_phantom (Key')

  type nonrec 'a t = (Nothing.t, 'a) t
end

module Int_table = Make (Int)
