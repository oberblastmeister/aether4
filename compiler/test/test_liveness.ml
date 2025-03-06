open Std
open Aether4
module Driver = Ae_driver
module C0 = Ae_c0_std
module Tir = Ae_tir_std

let check s =
  let tir = Driver.compile_source_to_tir s |> Or_error.ok_exn in
  let pred_table = Tir.Func.pred_table tir in
  let live_in, live_out = Tir.Liveness.compute_non_ssa ~pred_table tir in
  print_s
    [%message (live_in : Tir.Liveness.Live_set.t) (live_out : Tir.Liveness.Live_set.t)];
  ()
;;

let%expect_test "smoke" =
  check
    {|
    int main() {
        int first = 1342;
        int unused = 0;
        int second = 1234;
        int third = first + second;
        bool another = true;
        bool bl = false;
        if (another) {
          first += third;
        } else {
          another = bl;
        }
        return first + second;
    }
  |};
  [%expect
    {|
    ((live_in
      ((join@0 (second@25)) (then@1 (first@21 second@25 third@29))
       (else@2 (first@21 second@25 bl@33))))
     (live_out
      ((then@1 (second@25)) (else@2 (second@25))
       (start@3 (first@21 second@25 third@29 bl@33)))))
    |}]
;;
