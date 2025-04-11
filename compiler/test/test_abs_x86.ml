open Test_abs_x86_import
open Make_intern ()

let%expect_test _ =
  let instr =
    Instr.Jump { Block_call.label = lab "bruh"; args = [ Temp (temp "first") ] }
  in
  let uses = Instr.iter_uses instr |> Iter.to_list in
  print_s [%message (uses : Temp.t list)];
  [%expect {| (uses (first@0)) |}]
;;
