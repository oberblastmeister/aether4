module Caml_unix = Unix
include Core
include Functional
module Vec = Std_vec

module String = struct
  include Core.String

  let split_on t ~on = String.Search_pattern.split_on (String.Search_pattern.create on) t

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
let todo ?loc () = raise_s [%message "TODO" (loc : Source_code_position.t option)]
let todol loc = raise_s [%message "TODO" (loc : Source_code_position.t)]
