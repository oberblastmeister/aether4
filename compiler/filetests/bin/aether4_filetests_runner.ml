open Std
module Filetests = Aether4_filetests
module Path = Eio.Path
module Stdenv = Eio.Stdenv

let () =
  let@ env = Eio_main.run in
  let filetests_path = Path.(Stdenv.fs env / "compiler/filetests") in
  if not (String.equal (Filename.basename (Stdlib.Sys.getcwd ())) "aether4")
  then raise_s [%message "Must run filetests in the workspace root!"];
  Filetests.Run.run_tests env filetests_path;
  ()
;;
