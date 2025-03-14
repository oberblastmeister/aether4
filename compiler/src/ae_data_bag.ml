open Std

type 'a t =
  | Empty
  | Leaf of 'a
  | Append of 'a t * 'a t
  | List of 'a list

let append_list t l = Append (t, List l)
let append t t' = Append (t, t')
let of_list l = List l

let of_option = function
  | None -> Empty
  | Some x -> Leaf x
;;

let to_list builder =
  let rec go stack acc =
    match stack with
    | t :: ts ->
      (match t with
       | Empty -> go ts acc
       | Leaf x -> go ts (x :: acc)
       | List xs -> go ts (List.rev_append xs acc)
       | Append (t1, t2) -> go (t1 :: t2 :: ts) acc)
    | [] -> acc
  in
  go [ builder ] [] |> List.rev
;;

let to_arrayp t = to_list t |> Arrayp.of_list
let sexp_of_t f t = to_list t |> List.sexp_of_t f
let concat ts = List.fold_left ~init:Empty ~f:append ts

let rec map t ~f =
  match t with
  | Empty -> Empty
  | Leaf x -> Leaf (f x)
  | Append (l, r) -> Append (map l ~f, map r ~f)
  | List l -> List (List.map ~f l)
;;

let empty = Empty

module Syntax = struct
  let ( +> ) = append_list
  let ( <+ ) l t = Append (List l, t)
  let ( ++ ) = append
end

let%expect_test _ =
  let open Syntax in
  let b =
    of_list [ 1; 2; 3 ]
    +> [ 3; 4; 5 ]
    +> [ 1 ]
    ++ (of_list [ 3; 4; 5 ] +> [ 1234 ] ++ of_list [ 1; 2; 3 ] +> [ 12 ])
  in
  print_s [%sexp (b : int t)];
  ();
  [%expect {| (1 2 3 3 4 5 1 3 4 5 1234 1 2 3 12) |}]
;;
