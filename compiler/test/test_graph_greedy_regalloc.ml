(* TODO: fix these tests *)
open Std
open Aether4
module Entity = Ae_entity_std
module Ident = Entity.Ident
module Temp_entity = Ae_abs_asm_temp_entity
module Temp = Ae_abs_asm_temp_entity.Ident
module Intern = Entity.Intern.String_to_name.Make_global (Temp_entity.Witness) ()
module Regalloc = Ae_graph_greedy_regalloc
module Graph = Regalloc.Graph

let temp = Intern.intern

(* let%expect_test "simple no conflicts" =
  let graph = Graph.create () in
  Graph.add graph (temp "first");
  Graph.add graph (temp "second");
  Graph.add graph (temp "third");
  let res = Regalloc.color_graph graph Ident.Set.empty in
  print_s [%message (res : int Temp.Table.t * int)];
  ();
  [%expect {| (res (((first@0 0) (second@1 0) (third@2 0)) 0)) |}]
;;

let%expect_test "simple conflicts" =
  let graph = Graph.create () in
  Graph.add graph (temp "first");
  Graph.add graph (temp "second");
  Graph.add graph (temp "third");
  Graph.add_edge graph (temp "first") (temp "second");
  let res = Regalloc.color_graph graph Ident.Set.empty in
  print_s [%message (res : int Temp.Table.t * int)];
  ();
  [%expect {| (res (((first@0 1) (second@1 0) (third@2 0)) 1)) |}]
;;

let%expect_test "simple conflicts 2" =
  let graph = Graph.create () in
  Graph.add graph (temp "first");
  Graph.add graph (temp "second");
  Graph.add graph (temp "third");
  Graph.add graph (temp "fourth");
  Graph.add_edge graph (temp "first") (temp "second");
  Graph.add_edge graph (temp "second") (temp "third");
  Graph.add_edge graph (temp "third") (temp "first");
  Graph.add_edge graph (temp "fourth") (temp "first");
  let res = Regalloc.color_graph graph Ident.Set.empty in
  print_s [%message (res : int Temp.Table.t * int)];
  [%expect {| (res (((first@0 2) (second@1 1) (third@2 0) (fourth@3 0)) 2)) |}]
;;

let%expect_test "precolored" =
  let graph = Graph.create () in
  Graph.add graph (temp "first");
  Graph.add graph (temp "rdx");
  Graph.add_edge graph (temp "first") (temp "rdx");
  let res = Regalloc.color_graph graph (Ident.Set.singleton (temp "rdx")) in
  print_s [%message (res : int Temp.Table.t * int)];
  [%expect {| (res (((first@0 1) (rdx@4 0)) 1)) |}]
;; *)
