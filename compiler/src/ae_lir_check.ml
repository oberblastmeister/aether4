open Std
open Ae_lir_types

let check func =
  let open Or_error.Let_syntax in
  let%bind () = Check.check func in
  return ()
;;
