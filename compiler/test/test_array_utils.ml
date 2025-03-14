open Std
open Aether4
open Ae_array_utils

let check inserts array =
  let new_array = apply_inserts "should not be there!!!" inserts array in
  print_s [%sexp (new_array : string array)]
;;

let%expect_test "smoke" =
  check [] [||];
  [%expect {| () |}];
  check [ 0, "0"; 4, "awef"; 4, "xxx"; 2, "bruh"; 3, "3" ] [| "a"; "b"; "c"; "d" |];
  [%expect {| (0 a b bruh c 3 d awef xxx) |}];
  check [ 4, "awef"; 3, "3"; 2, "bruh"; 4, "xxx" ] [| "a"; "b"; "c"; "d" |];
  [%expect {| (a b bruh c 3 d awef xxx) |}];
  check [ 0, "0" ] [||];
  [%expect {| (0) |}];
  check [ 0, "0"; 1, "1"; 2, "2"; 0, "0'"; 1, "1'" ] [||];
  [%expect {| (0 0' 1 1' 2) |}];
  ()
;;
