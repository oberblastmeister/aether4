open Std
open Aether4
module Structures = Ae_structures
module Graph = Ae_data_graph_std

module Dfs =
  Graph.Dfs_gen.Make (Structures.String_mutable_map) (Structures.String_mutable_set)

let%expect_test "smoke" =
  let graph =
    String.Map.of_alist_exn
      [ "a", [ "b"; "x" ]; "b", [ "c" ]; "c", [ "d" ]; "d", [ "a" ] ]
    |> Graph.of_map_list
  in
  let res = Dfs.find_cycle ~equal:String.equal graph |> Option.value_exn in
  print_s [%message (res : string * string * string list)];
  [%expect {| (res (d a (a b c d a))) |}]
;;
