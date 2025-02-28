open Std
open Aether4
module Driver = Ae_driver
module C0 = Ae_c0_std
module Tir = Ae_tir_std

let check s =
  let res = Driver.compile_source_to_tir s |> Or_error.ok_exn in
  print_s [%sexp (res : Tir.Func.t)];
  ()
;;

(* let%expect_test "first" =
  check
    {|
    int main() {
      int first = 0;
      int second = 1234;
      bool third = true;
      return first + second;
    }
  |}
;; *)
