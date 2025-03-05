open Std
open Aether4
module Entity = Ae_entity_std
module Ident = Entity.Ident
module Vreg_entity = Ae_vreg_entity
module Vreg = Ae_vreg_entity.Ident
module Intern = Entity.Intern.String_to_name.Make_global (Vreg_entity.Witness) ()
module Regalloc = Ae_graph_greedy_regalloc
module Graph = Regalloc.Graph

let vreg = Intern.intern

let%expect_test "simple no conflicts" =
  let graph = Graph.create () in
  Graph.add graph (vreg "first");
  Graph.add graph (vreg "second");
  Graph.add graph (vreg "third");
  let res = Regalloc.color_graph graph Ident.Set.empty in
  print_s [%message (res : int Vreg.Table.t * int)];
  ();
  [%expect {| (res (((first@0 0) (second@1 0) (third@2 0)) 0)) |}]
;;

let%expect_test "simple conflicts" =
  let graph = Graph.create () in
  Graph.add graph (vreg "first");
  Graph.add graph (vreg "second");
  Graph.add graph (vreg "third");
  Graph.add_edge graph (vreg "first") (vreg "second");
  let res = Regalloc.color_graph graph Ident.Set.empty in
  print_s [%message (res : int Vreg.Table.t * int)];
  ();
  [%expect {| (res (((first@0 1) (second@1 0) (third@2 0)) 1)) |}]
;;

let%expect_test "simple conflicts 2" =
  let graph = Graph.create () in
  Graph.add graph (vreg "first");
  Graph.add graph (vreg "second");
  Graph.add graph (vreg "third");
  Graph.add graph (vreg "fourth");
  Graph.add_edge graph (vreg "first") (vreg "second");
  Graph.add_edge graph (vreg "second") (vreg "third");
  Graph.add_edge graph (vreg "third") (vreg "first");
  Graph.add_edge graph (vreg "fourth") (vreg "first");
  let res = Regalloc.color_graph graph Ident.Set.empty in
  print_s [%message (res : int Vreg.Table.t * int)];
  [%expect {| (res (((first@0 2) (second@1 1) (third@2 0) (fourth@3 0)) 2)) |}]
;;

let%expect_test "precolored" =
  let graph = Graph.create () in
  Graph.add graph (vreg "first");
  Graph.add graph (vreg "rdx");
  Graph.add_edge graph (vreg "first") (vreg "rdx");
  let res = Regalloc.color_graph graph (Ident.Set.singleton (vreg "rdx")) in
  print_s [%message (res : int Vreg.Table.t * int)];
  [%expect {| (res (((first@0 1) (rdx@4 0)) 1)) |}]
;;
