open Std
open Ae_intable

module type S = sig
  module Key : Intable

  type 'a t [@@deriving sexp_of]

  val create : ?size:int -> unit -> 'v t
  val find : 'v t -> Key.t -> 'v option
  val find_int : 'v t -> int -> 'v option
  val find_int_exn : 'v t -> int -> 'v
  val findi_int : 'v t -> int -> (Key.t * 'v) option
  val remove : 'v t -> Key.t -> unit
  val find_exn : 'v t -> Key.t -> 'v
  val find_or_add : 'v t -> Key.t -> default:(unit -> 'v) -> 'v
  val find_multi : 'v list t -> Key.t -> 'v list
  val set : 'v t -> key:Key.t -> data:'v -> unit
  val add_exn : 'v t -> key:Key.t -> data:'v -> unit
  val mem : 'v t -> Key.t -> bool
  val update : 'v t -> Key.t -> f:('v option -> 'v) -> unit
  val add_multi : 'v list t -> key:Key.t -> data:'v -> unit
  val of_list : (Key.t * 'v) list -> 'v t
  val to_list : 'v t -> (Key.t * 'v) list
  val of_iter : ?size:int -> (Key.t * 'v) Iter.t -> 'v t
  val iter_keys : 'v t -> Key.t Iter.t
  val iteri : 'v t -> (Key.t * 'v) Iter.t
  val length : 'v t -> int
  val max_index : 'v t -> int
  val map : 'a t -> f:('a -> 'b) -> 'b t

  val of_iter_accum
    :  ?size:int
    -> (Key.t * 'a) Iter.t
    -> init:'acc
    -> f:('acc -> 'a -> 'acc)
    -> 'acc t

  module Syntax : sig
    val ( .!() ) : 'a t -> Key.t -> 'a
    val ( .!?() ) : 'a t -> Key.t -> 'a option
    val ( .!()<- ) : 'a t -> Key.t -> 'a -> unit
  end
end

module Make (Key : Intable) : S with module Key = Key = struct
  module Key = Key

  open struct
    module OA = Option_array
  end

  type 'a t =
    { mutable a : (Key.t * 'a) Option_array.t
    ; mutable length : int
    ; mutable max_index : int
    }

  let length t = t.length
  let max_index t = t.max_index
  let create ?(size = 0) () = { a = OA.create ~len:size; length = 0; max_index = -1 }
  let size t = OA.length t.a

  let resize_for_index t index =
    if index >= size t then t.a <- Ae_data_utils.Option_array.resize_for_index t.a index
  ;;

  let find t k =
    let i = Key.to_int k in
    if i >= size t then None else Option.map ~f:snd @@ OA.get t.a @@ i
  ;;

  let find_int t i = if i >= size t then None else Option.map ~f:snd @@ OA.get t.a @@ i
  let findi_int t i = if i >= size t then None else OA.get t.a @@ i

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

  let key_not_found _t k = raise_s [%message "key not found" ~key:(k : Key.t)]
  let int_not_found _t i = raise_s [%message "key not found" ~key:(i : int)]

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

  let find_int_exn t i =
    if i >= size t
    then int_not_found t i
    else if OA.is_none t.a i
    then int_not_found t i
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
  let sexp_of_t f t = to_list t |> List.sexp_of_t (Tuple2.sexp_of_t Key.sexp_of_t f)

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
    then raise_s [%message "key is already inside" (key : Key.t)]
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
    let ( .!?() ) = find
    let ( .!() ) = find_exn
    let ( .!()<- ) t key data = set t ~key ~data
  end
end
