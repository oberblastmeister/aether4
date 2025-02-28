module Caml_sys = Sys
module Caml_unix = Unix
include Core
include Functional

module Vec = struct
  include Vec

  let fold_right vec ~init ~f =
    let rec go acc i = if i >= 0 then go (f (Vec.get vec i) acc) (i - 1) else acc in
    go init (Vec.length vec - 1)
  ;;
end

module Arrayp = struct
  include Array.Permissioned

  let iteri_rev t ~f =
    let i = ref (Array.Permissioned.length t - 1) in
    while !i > 0 do
      f !i (Array.Permissioned.get t !i);
      decr i;
      ()
    done;
    ()
  ;;

  let unchecked_coerce
    : 'a. ('a, _) Array.Permissioned.t -> ('a, [< _ perms ]) Array.Permissioned.t
    =
    Obj.magic
  ;;

  let unchecked_of_array a = unchecked_coerce (of_array_id a)
end

type 'a iarray = ('a, immutable) Arrayp.t

let sexp_of_iarray f (t : _ iarray) = Array.Permissioned.sexp_of_t f sexp_of_opaque t
let ( .@() ) = Arrayp.get
let ( .@()<- ) = Arrayp.set

module String = struct
  include Core.String

  let split_on t ~on = String.Search_pattern.split_on (String.Search_pattern.create on) t

  let lsplit2_on s ~on =
    let open Option.Let_syntax in
    let pat = String.Search_pattern.create on in
    let%bind i = String.Search_pattern.index pat ~in_:s in
    Some (String.subo ~len:i s, String.subo ~pos:(i + String.length on) s)
  ;;

  let lsplit2_on_exn s ~on =
    let pat = String.Search_pattern.create on in
    let i = String.Search_pattern.index_exn pat ~in_:s in
    String.subo ~len:i s, String.subo ~pos:(i + String.length on) s
  ;;

  let strip_prefix s ~prefix =
    if String.is_prefix s ~prefix
    then Some (String.drop_prefix s (String.length prefix))
    else None
  ;;

  let strip_suffix s ~suffix =
    if String.is_suffix s ~suffix
    then Some (String.drop_suffix s (String.length suffix))
    else None
  ;;
end

module Z = struct
  include Z

  let sexp_of_t i = Sexp.Atom (Z.to_string i)

  let t_of_sexp s =
    let s = String.t_of_sexp s in
    Z.of_string s
  ;;

  let of_string_exn = Z.of_string

  let of_string s =
    match Z.of_string s with
    | exception Invalid_argument _ -> None
    | z -> Some z
  ;;

  let to_int64_exn = Z.to_int64
  let to_int32 = Z.to_int32

  let to_int64 z =
    match Z.to_int64 z with
    | exception Overflow -> None
    | res -> Some res
  ;;

  let to_int32 z =
    match Z.to_int64 z with
    | exception Overflow -> None
    | res -> Some res
  ;;

  let compare = Z.compare
  let equal = Z.equal
end

let ( let@ ) f x = f x
let ( @> ) = Functional.Fold.( @> )
let for_ = Iter.for_
let todo ?loc () = raise_s [%message "TODO" (loc : Source_code_position.t option)]
let todol loc = raise_s [%message "TODO" (loc : Source_code_position.t)]
