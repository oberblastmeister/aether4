open Std
open Aether4
module Driver = Ae_driver
module C0 = Ae_c0_std
module Tir = Ae_tir_std

let check s =
  let tir = Driver.compile_source_to_tir s |> Or_error.ok_exn in
  let pred_table = Tir.Func.pred_table tir in
  let live_in, live_out = Tir.Liveness.compute_non_ssa pred_table tir in
  ()
;;

(* let%expect_test "smoke" =
  check {|
    int main() {
      int first = 1342;
      int second = 1234;
      int third = first + second;
    }
  |} *)