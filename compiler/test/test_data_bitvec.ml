open Std
open Aether4
module Bitvec = Ae_data_bitvec

let%expect_test _ =
  let b = Bitvec.create () in
  print_s [%sexp (b : Bitvec.t)];
  [%expect {| () |}];
  let b = Bitvec.create ~size:10 () in
  print_s [%sexp (b : Bitvec.t)];
  [%expect {| () |}];
  let b = Bitvec.create ~size:10 ~default:true () in
  print_s [%sexp (b : Bitvec.t)];
  [%expect
    {|
    (9 8 7 6 5 4 3 2 1 0)
    |}];
  let b = Bitvec.create () in
  Bitvec.add b 5;
  print_s [%sexp (b : Bitvec.t)];
  [%expect {| (5) |}]
;;
