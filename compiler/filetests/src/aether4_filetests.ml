open Std
open Filetests_run
module Run = Filetests_run

open struct
  module Path = Eio.Path
  module Stdenv = Eio.Stdenv
end

let%test_unit "filetests" =
  let@ env = Eio_main.run in
  let filetests_path = Path.(Stdenv.fs env / "../") in
  run_tests env filetests_path;
  ()
;;
