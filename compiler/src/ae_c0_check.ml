open Std
module Check_init = Ae_c0_check_init
module Check_return = Ae_c0_check_return

let check_program program =
  let open Or_error.Let_syntax in
  let%bind () = Check_return.check_program program in
  let%bind () = Check_init.check_program program in
  Ok ()
;;
