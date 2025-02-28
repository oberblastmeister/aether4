open Std
module Signatures = Ae_signatures
module OA = Option_array
open Ae_entity_sigs

module type S_phantom_without_make = sig
  module Key : Key_phantom

  type ('w, 'a) t

  val create : ?size:int -> unit -> ('w, 'v) t
  val find : ('w, 'v) t -> 'w Key.t -> 'v option
  val remove : ('w, 'v) t -> 'w Key.t -> unit
  val find_exn : ('w, 'v) t -> 'w Key.t -> 'v
  val find_or_add : ('w, 'v) t -> 'w Key.t -> default:(unit -> 'v) -> 'v
  val find_multi : ('w, 'v list) t -> 'w Key.t -> 'v list
  val set : ('w, 'v) t -> key:'w Key.t -> data:'v -> unit
  val add_exn : ('w, 'v) t -> key:'w Key.t -> data:'v -> unit
  val mem : ('w, 'v) t -> 'w Key.t -> bool
  val update : ('w, 'v) t -> 'w Key.t -> f:('v option -> 'v) -> unit
  val add_multi : ('w, 'v list) t -> key:'w Key.t -> data:'v -> unit
  val of_list : ('w Key.t * 'v) list -> ('w, 'v) t
  val to_list : ('w, 'v) t -> ('w Key.t * 'v) list
  val of_iter : ?size:int -> ('w Key.t * 'v) Iter.t -> ('w, 'v) t
  val iter_keys : ('w, 'v) t -> 'w Key.t Iter.t
  val iteri : ('w, 'v) t -> ('w Key.t * 'v) Iter.t
  val length : ('w, 'v) t -> int
  val max_index : ('w, 'v) t -> int
  val map : ('w, 'a) t -> f:('a -> 'b) -> ('w, 'b) t

  val of_iter_accum
    :  ?size:int
    -> ('w Key.t * 'a) Iter.t
    -> init:'acc
    -> f:('acc -> 'a -> 'acc)
    -> ('w, 'acc) t

  module Syntax : sig
    val ( .!() ) : ('w, 'a) t -> 'w Key.t -> 'a
    val ( .!()<- ) : ('w, 'a) t -> 'w Key.t -> 'a -> unit
  end

  module Perms : sig
    type ('w, 'a, -'perms) t

    val create : ?size:int -> unit -> ('w, 'v, [< _ perms ]) t
    val find : ('w, 'v, [> read ]) t -> 'w Key.t -> 'v option
    val find_exn : ('w, 'v, [> read ]) t -> 'w Key.t -> 'v
    val remove : ('w, 'v, [> write ]) t -> 'w Key.t -> unit

    val find_or_add
      :  ('w, 'v, [> read_write ]) t
      -> 'w Key.t
      -> default:(unit -> 'v)
      -> 'v

    val set : ('w, 'v, [> write ]) t -> key:'w Key.t -> data:'v -> unit
    val add_exn : ('w, 'v, [> write ]) t -> key:'w Key.t -> data:'v -> unit
    val mem : ('w, 'v, [> read ]) t -> 'w Key.t -> bool
    val update : ('w, 'v, [> read_write ]) t -> 'w Key.t -> f:('v option -> 'v) -> unit
    val of_list : ('w Key.t * 'v) list -> ('w, 'v, [< _ perms ]) t
    val of_iter : ?size:int -> ('w Key.t * 'v) Iter.t -> ('w, 'v, [< _ perms ]) t
    val iter_keys : ('w, 'v, [> read ]) t -> 'w Key.t Iter.t
    val iteri : ('w, 'v, [> read ]) t -> ('w Key.t * 'v) Iter.t
    val length : ('w, 'v, _) t -> int

    module Syntax : sig
      val ( .!() ) : ('w, 'a, [> read ]) t -> 'w Key.t -> 'a
      val ( .!()<- ) : ('w, 'a, [> write ]) t -> 'w Key.t -> 'a -> unit
    end
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
  module Key = Key

  module T = struct
    type ('w, 'a) t =
      { mutable a : ('w Key.t * 'a) Option_array.t
      ; mutable length : int
      ; mutable max_index : int
      }

    let length t = t.length
    let max_index t = t.max_index

    let sexp_of_t t f g =
      Option_array.sexp_of_t (Tuple2.sexp_of_t (Key.sexp_of_t f) g) t.a
    ;;

    let create ?(size = 0) () = { a = OA.create ~len:size; length = 0; max_index = -1 }
    let size t = OA.length t.a

    let resize_for_index t index =
      if index >= size t then t.a <- Ae_data_utils.Option_array.resize_for_index t.a index
    ;;

    let find t k =
      let i = Key.to_int k in
      if i >= size t then None else Option.map ~f:snd @@ OA.get t.a @@ i
    ;;

    let find_multi t k =
      match find t k with
      | None -> []
      | Some l -> l
    ;;

    let remove t k =
      let i = Key.to_int k in
      if i < size t
      then (
        OA.set_none t.a i;
        t.length <- t.length - 1;
        ())
    ;;

    let mem t k =
      let i = Key.to_int k in
      i < size t && OA.is_some t.a i
    ;;

    let key_not_found _t k = raise_s [%message "key not found" ~key:(k : _ Key.t)]

    let clear t =
      Option_array.clear t.a;
      t.length <- 0;
      t.max_index <- -1;
      ()
    ;;

    let find_exn t k =
      let i = Key.to_int k in
      if i >= size t
      then key_not_found t k
      else if OA.is_none t.a i
      then key_not_found t k
      else snd @@ OA.get_some_exn t.a i
    ;;

    let set t ~key:k ~data:v =
      let index = Key.to_int k in
      resize_for_index t index;
      if OA.is_none t.a index
      then (
        t.length <- t.length + 1;
        t.max_index <- max t.max_index index);
      OA.set_some t.a index (k, v)
    ;;

    let find_or_add t k ~default =
      if mem t k
      then find_exn t k
      else (
        let res = default () in
        set t ~key:k ~data:res;
        res)
    ;;

    let foldi t ~init ~f =
      OA.fold t.a ~init ~f:(fun z -> Option.value_map ~default:z ~f:(fun x -> f z x))
    ;;

    let iteri t = Container.iter ~fold:foldi t

    let fold t ~init ~f =
      OA.fold t.a ~init ~f:(fun z -> Option.value_map ~default:z ~f:(fun (_, v) -> f z v))
    ;;

    let to_list t = Container.to_list ~fold:foldi t

    let of_list l =
      let t = create () in
      List.iter l ~f:(fun (k, v) -> set t ~key:k ~data:v);
      t
    ;;

    let of_iter ?size i =
      let t = create ?size () in
      Iter.iter i ~f:(fun (k, v) -> set t ~key:k ~data:v);
      t
    ;;

    let sexp_of_t f g t = to_list t |> List.sexp_of_t (Tuple2.sexp_of_t f g)
    let update t k ~f = set t ~key:k ~data:(f (find t k))

    let add_multi t ~key ~data =
      update t key ~f:(function
        | None -> [ data ]
        | Some l -> data :: l)
    ;;

    let of_iter_accum ?size i ~init ~f =
      let t = create ?size () in
      Iter.iter i ~f:(fun (k, v) ->
        update t k ~f:(function
          | None -> f init v
          | Some acc -> f acc v));
      t
    ;;

    let add_exn t ~key ~data =
      if mem t key
      then raise_s [%message "key is already inside" (key : _ Key.t)]
      else set t ~key ~data
    ;;

    let iter_keys t ~f =
      Option_array.iter t.a |> Iter.filter_map ~f:Fn.id |> Iter.map ~f:fst |> Iter.iter ~f
    ;;

    let iteri t ~f = Option_array.iter t.a |> Iter.filter_map ~f:Fn.id |> Iter.iter ~f

    let of_iter_accum ?size i ~init ~f =
      let t = create ?size () in
      Iter.iter i ~f:(fun (k, v) ->
        update t k ~f:(function
          | None -> f init v
          | Some acc -> f acc v));
      t
    ;;

    let map t ~f =
      let t' = create ~size:(Option_array.length t.a) () in
      iteri t ~f:(fun (k, v) -> set t' ~key:k ~data:(f v));
      t'
    ;;

    module Syntax = struct
      let ( .!() ) = find_exn
      let ( .!()<- ) t key data = set t ~key ~data
    end
  end

  module Perms = struct
    include T

    type nonrec ('w, 'a, -'perms) t = ('w, 'a) t
  end

  include T

  module Make (Witness : Ae_entity_witness.S) = struct
    type nonrec 'v t = (Witness.t, 'v) t

    let sexp_of_t f t = sexp_of_t (Key.sexp_of_t sexp_of_opaque) f t
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
