(* TODO: fix this module *)
(* open Std
open Aether4
module Bounded_heap = Ae_bounded_heap

module Id = Entity.Id
module Temp_entity = Ae_abs_asm_temp_entity
module Temp_id = Temp_entity.Id
module Table = Id.Table

let%expect_test "smoke" =
  let heap = Bounded_heap.create ~weight_bound:100 () in
  let id = Id.unchecked_of_int in
  let check () =
    print_s [%sexp (Bounded_heap.remove_max heap : (Temp_id.t * int) option)]
  in
  Bounded_heap.add_exn heap (id 0) 0;
  check ();
  [%expect {| ((0 0)) |}];
  Bounded_heap.add_exn heap (id 0) 0;
  check ();
  [%expect {| ((0 0)) |}];
  Bounded_heap.add_exn heap (id 4) 4;
  Bounded_heap.add_exn heap (id 5) 5;
  Bounded_heap.add_exn heap (id 6) 6;
  Bounded_heap.add_exn heap (id 7) 4;
  Bounded_heap.add_exn heap (id 8) 4;
  Bounded_heap.remove_exn heap (id 7);
  Bounded_heap.add_exn heap (id 9) 5;
  Bounded_heap.add_exn heap (id 10) 5;
  Bounded_heap.add_exn heap (id 11) 5;
  Bounded_heap.add_exn heap (id 12) 5;
  Bounded_heap.add_exn heap (id 13) 5;
  Bounded_heap.remove_exn heap (id 9);
  Bounded_heap.remove_exn heap (id 12);
  check ();
  [%expect {| ((8 4)) |}];
  check ();
  [%expect {| ((4 4)) |}];
  check ();
  [%expect {| ((13 5)) |}];
  Bounded_heap.increase_exn heap (id 5) 10;
  check ();
  [%expect {| ((11 5)) |}];
  check ();
  [%expect {| ((10 5)) |}];
  check ();
  [%expect {| ((6 6)) |}];
  check ();
  [%expect {| ((5 15)) |}];
  check ();
  [%expect {| () |}];
  ()
;; *)
