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
      ((join@0 (first@3 second@4)) (then@1 (first@3 second@4 third@9))
       (else@2 (first@3 second@4 bl@10))))
     (live_out
      ((then@1 (first@3 second@4)) (else@2 (first@3 second@4))
       (start@3 (first@3 second@4 third@9 bl@10)))))
    |}]
;;
