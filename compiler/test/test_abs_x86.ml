open Test_abs_x86_import
open Make_intern ()

let%expect_test _ =
  let instr =
    Instr.Jump { Block_call.label = lab "bruh"; args = [ Vreg (vreg "first") ] }
  in
  let uses = Instr.iter_uses instr |> Iter.to_list in
  print_s [%message (uses : Vreg.t list)];
  [%expect {| (uses (first@0)) |}]
;;
