(* TODO, doesn't necessarily have to be entity baesd, move to different namespace
*)
open Std
module Signatures = Ae_signatures

type 'a t = 'a Option_array.t

module OA = Option_array
include Ae_entity_map_intf

module Make (Key : Key) : S with module Key = Key = struct
  module Key = Key

  type ('w, 'a) t = { mutable a : ('w Key.t * 'a) Option_array.t }

  let create ?(size = 0) () = { a = OA.create ~len:size }
  let size t = OA.length t.a

  let resize_for_index t index =
    if index >= size t then t.a <- Ae_data_utils.Option_array.resize_for_index t.a index
  ;;

  let find t k =
    let i = Key.to_int k in
    if i >= size t then None else Option.map ~f:snd @@ OA.get t.a @@ i
  ;;

  let remove t k =
    let i = Key.to_int k in
    if i < size t then OA.set_none t.a i
  ;;

  let mem t k =
    let i = Key.to_int k in
    i < size t && OA.is_some t.a i
  ;;

  let key_not_found _t k = raise_s [%message "key not found" ~key:(k : _ Key.t)]
  let clear t = Option_array.clear t.a

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
    Iter.iter i ~f:(fun (k, v) ->
      (* if mem ~to_int t k then raise_s [%message "key was present twice"]; *)
      set t ~key:k ~data:v);
    t
  ;;

  let sexp_of_t f g t = to_list t |> List.sexp_of_t (Tuple2.sexp_of_t f g)
  let update t k ~f = set t ~key:k ~data:(f (find t k))

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
end
