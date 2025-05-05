open Std
open Aether4
open Ae_struct_layout

let%expect_test "smoke" =
  let res =
    calculate
      Field.
        [ { size = 1; align = 1 }
        ; { size = 1; align = 1 }
        ; { size = 4; align = 4 }
        ; { size = 4; align = 2 }
        ; { size = 16; align = 16 }
        ; { size = 1; align = 1 }
        ; { size = 1; align = 1 }
        ; { size = 16; align = 16 }
        ; { size = 1; align = 1 }
        ]
  in
  print_s [%message (res : t)];
  [%expect {| (res ((offsets (0 1 4 8 16 32 33 48 64)) (size 80) (align 16))) |}]
;;
