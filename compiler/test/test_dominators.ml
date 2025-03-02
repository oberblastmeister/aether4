open Std
open Aether4
module Dominators = Ae_dominators
module Entity = Ae_entity_std
module Ident = Entity.Ident
module Label_entity = Ae_label_entity
module Label = Label_entity.Ident
module Graph = Ae_data_graph_std
module Intern = Entity.Intern.String_to_name.Make_global (Label_entity.Witness) ()
module Entity_graph_utils = Ae_entity_graph_utils

let lab = Intern.intern

let graph xs =
  let succ_table =
    xs |> List.map ~f:(fun (n, ns) -> lab n, List.map ~f:lab ns) |> Ident.Table.of_list
  in
  let g = Entity_graph_utils.graph_of_adj_table succ_table in
  Entity_graph_utils.to_bi g
;;

type test =
  { idoms : Dominators.Immediate.t
  ; frontier : Dominators.Frontier.t
  ; domtree : Dominators.Tree.t
  }
[@@deriving sexp_of]

let run_test g =
  let idoms = Dominators.Immediate.compute ~start:(lab "start") g in
  let frontier = Dominators.Frontier.compute idoms g in
  let domtree = Dominators.Tree.of_immediate idoms in
  let test = { idoms; frontier; domtree } in
  print_s (sexp_of_test test)
;;

(* Figure 2 *)
let%expect_test _ =
  let g =
    graph
      [ "start", [ "4"; "3" ]; "4", [ "1" ]; "3", [ "2" ]; "2", [ "1" ]; "1", [ "2" ] ]
  in
  run_test g;
  [%expect
    {|
    ((idoms ((4@0 start@2) (3@1 start@2) (1@3 start@2) (2@4 start@2)))
     (frontier ((4@0 (1@3)) (3@1 (2@4)) (1@3 (2@4)) (2@4 (1@3))))
     (domtree ((start@2 (2@4 1@3 3@1 4@0)))))
    |}]
;;

(* Figure 4 *)
let%expect_test _ =
  let g =
    graph
      [ "start", [ "5"; "4" ]
      ; "5", [ "1" ]
      ; "4", [ "2"; "3" ]
      ; "1", [ "2" ]
      ; "2", [ "1"; "3" ]
      ; "3", [ "2" ]
      ]
  in
  run_test g;
  [%expect
    {|
    ((idoms
      ((4@0 start@2) (3@1 start@2) (1@3 start@2) (2@4 start@2) (5@5 start@2)))
     (frontier
      ((4@0 (3@1 2@4)) (3@1 (2@4)) (1@3 (2@4)) (2@4 (3@1 1@3)) (5@5 (1@3))))
     (domtree ((start@2 (5@5 2@4 1@3 3@1 4@0)))))
    |}]
;;
