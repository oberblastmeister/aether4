open Std
open Aether4
module Tir = Ae_tir_std
open Tir
module Entity = Ae_entity_std
module Temp_intern = Entity.Intern.String_to_name.Make_global (Tir.Temp_entity.Witness) ()
module Generic_ir = Ae_generic_ir_std

let temp = Temp_intern.intern

module Sequentialize_parallel_moves = Ae_sequentialize_parallel_moves.Make (Tir)
module Move = Sequentialize_parallel_moves.Move

let rsi = temp "rsi"
let rdi = temp "rdi"
let rdx = temp "rdx"
let rcx = temp "rcx"
let rax = temp "rax"
let rbx = temp "rbx"
let r8 = temp "r8"
let r9 = temp "r9"
let r10 = temp "r10"
let r11 = temp "r11"
let a = temp "a"
let b = temp "b"
let c = temp "c"
let d = temp "d"
let scratch = temp "scratch"

let sequentialize par_move =
  Sequentialize_parallel_moves.sequentialize
    ~in_same_reg:Temp.equal
    ~get_scratch:(fun () -> scratch)
    par_move
;;

let pmov dsts srcs =
  List.zip_exn dsts srcs |> List.map ~f:(fun (dst, src) -> { Move.dst; src; ty = Int })
;;

let check dsts srcs =
  let res = sequentialize (pmov dsts srcs) in
  let res = List.map res ~f:(fun move -> move.dst.name, move.src.name) in
  print_s [%sexp (res : (string * string) list)]
;;

let%expect_test "simple no scratch" =
  check [ b; d; c ] [ a; a; b ];
  [%expect {| ((c b) (b a) (d a)) |}]
;;

let%expect_test "two" =
  check [ b; d ] [ a; b ];
  [%expect {| ((d b) (b a)) |}];
  ()
;;

let%expect_test "simple scratch" =
  check [ b; d; c; a ] [ a; a; b; c ];
  [%expect {| ((scratch a) (d a) (a c) (c b) (b scratch)) |}]
;;

let%expect_test "repetition" =
  check [ a; b; c; d ] [b; a; a; a];
  [%expect {| ((scratch b) (b a) (c a) (d a) (a scratch)) |}]

let%expect_test "multiple components" =
  check [ rdi; rsi; rdx; rcx; r8; r9 ] [ rsi; rdi; rsi; rsi; r9; r8 ];
  [%expect
    {|
    ((scratch rsi) (rdx rsi) (rcx rsi) (rsi rdi) (rdi scratch) (scratch r9)
     (r9 r8) (r8 scratch))
    |}]
;;
