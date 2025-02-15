open Std
open Aether4
module Entity = Ae_entity_std
module Name = Entity.Name
module Vreg_entity = Ae_vreg_entity
module Vreg = Ae_vreg_entity.Name
module Intern = Entity.Intern.String_to_name.Make_global (Vreg_entity.Id.Witness) ()
module Regalloc = Ae_graph_greedy_regalloc
module Graph = Regalloc.Graph
module Color = Regalloc.Color

let vreg = Intern.intern

let%expect_test "simple no conflicts" =
  let graph = Graph.create () in
  Graph.add graph (vreg "first");
  Graph.add graph (vreg "second");
  Graph.add graph (vreg "third");
  let res = Regalloc.color_graph graph Name.Set.empty in
  print_s [%message (res : Color.t Vreg.Table.t * Color.t)];
  ();
  [%expect {| (res (((first@0 0) (second@1 0) (third@2 0)) 0)) |}]
;;

let%expect_test "simple conflicts" =
  let graph = Graph.create () in
  Graph.add graph (vreg "first");
  Graph.add graph (vreg "second");
  Graph.add graph (vreg "third");
  Graph.add_edge graph (vreg "first") (vreg "second");
  let res = Regalloc.color_graph graph Name.Set.empty in
  print_s [%message (res : Color.t Vreg.Table.t * Color.t)];
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
  let res = Regalloc.color_graph graph Name.Set.empty in
  print_s [%message (res : Color.t Vreg.Table.t * Color.t)];
  [%expect {| (res (((first@0 2) (second@1 1) (third@2 0) (fourth@3 0)) 2)) |}]
;;
