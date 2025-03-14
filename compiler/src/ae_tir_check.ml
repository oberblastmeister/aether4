open Std
open Ae_tir_types

let check func =
  let open Or_error.Let_syntax in
  let%bind () = Check_ssa.check func in
  return ()
;;
