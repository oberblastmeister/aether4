open Std
open Aether4
module Bitset = Ae_data_bitset

let%expect_test _ =
  let b = Bitset.create () in
  print_s [%sexp (b : Bitset.t)];
  [%expect {| () |}];
  let b = Bitset.create ~size:10 () in
  print_s [%sexp (b : Bitset.t)];
  [%expect {| () |}];
  let b = Bitset.create ~size:10 ~default:true () in
  print_s [%sexp (b : Bitset.t)];
  [%expect
    {|
    (9 8 7 6 5 4 3 2 1 0)
    |}];
  let b = Bitset.create () in
  Bitset.add b 5;
  print_s [%sexp (b : Bitset.t)];
  [%expect {| (5) |}]
;;
