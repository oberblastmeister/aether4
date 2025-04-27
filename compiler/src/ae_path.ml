open Std

let remove_trailing_slashes s =
  let open Stdlib in
  let rec aux i =
    if i <= 1 || s.[i - 1] <> '/'
    then if i = String.length s then s else String.sub s 0 i
    else aux (i - 1)
  in
  aux (String.length s)
;;

let%expect_test "bruh" =
  let p = Filename.dirname "awef" in
  print_s [%sexp (p : string)];
  [%expect {| . |}];
  let p = Filename.dirname "/awef" in
  print_s [%sexp (p : string)];
  [%expect {| / |}];
  let p = Filename.dirname "/" in
  print_s [%sexp (p : string)];
  [%expect {| / |}];
  let p = Filename.dirname "." in
  print_s [%sexp (p : string)];
  [%expect {| . |}];
  let p = Filename.dirname "///" in
  print_s [%sexp (p : string)];
  [%expect {| / |}];
  let p = Filename.dirname "" in
  print_s [%sexp (p : string)];
  [%expect {| . |}]
;;

include Filename

let ( / ) = concat

let parent p =
  let p' = dirname p in
  if p' = root || p' = "." then None else Some (dirname p)
;;

let is_file = Sys_unix.is_file_exn
let is_directory = Sys_unix.is_directory_exn
