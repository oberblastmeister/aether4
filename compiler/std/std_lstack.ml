open Core

type 'a t = 'a list ref

let create () = ref []
let push t x = t := x :: !t
let append_list t l = t := List.rev_append l !t

let pop t =
  match !t with
  | [] -> None
  | x :: xs ->
    t := xs;
    Some x
;;

let pop_exn t =
  match !t with
  | [] -> raise_s [%message "Empty stack!"]
  | x :: xs ->
    t := xs;
    x
;;

let to_list t = List.rev !t
let to_list_rev t = !t
