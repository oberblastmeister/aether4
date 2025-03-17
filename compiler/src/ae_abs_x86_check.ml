open Std
open Ae_abs_x86_types

let check func =
  let open Or_error.Let_syntax in
  let%bind () = Check_well_formed.check func in
  let%bind () = Check_ssa.check func in
  return ()
;;
