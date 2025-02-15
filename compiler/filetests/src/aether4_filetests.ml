open Std
open Aether4
module Driver = Ae_driver
module Stdenv = Eio.Stdenv
module Path = Eio.Path
module Process = Eio.Process
module File = Eio.File
module Flow = Eio.Flow

let run_test env workspace_root_path tests_path (name, path) =
  let module Digest = Digestif.BLAKE2B in
  let@ file = Path.with_open_in path in
  let source = Flow.read_all file in
  let digest = Digest.digestv_string [ name; source ] in
  let cache_dir_path = Path.(workspace_root_path / ".filetests_cache") in
  Path.mkdirs ~exists_ok:true ~perm:0o777 cache_dir_path;
  let hashed_dir_path =
    Path.(
      cache_dir_path
      / (Filename.basename (Filename.chop_extension (Path.native_exn path))
         ^ "-"
         ^ Base64.encode_exn (Digest.to_raw_string digest)))
  in
  let asm_path = Path.(hashed_dir_path / (name ^ ".s")) in
  Path.mkdirs ~exists_ok:true ~perm:0o777 hashed_dir_path;
  Digest.pp Stdlib.Format.std_formatter digest;
  if Path.is_file Path.(hashed_dir_path / "a.out")
  then ()
  else (
    let asm_content = Driver.compile_source_to_asm source in
    let out_path = Path.(hashed_dir_path / "a.out") in
    let@ asm_file = Path.with_open_out ~create:(`Or_truncate 0o777) asm_path in
    Flow.copy_string asm_content asm_file;
    Driver.link_files_with_runtime
      (Stdenv.process_mgr env)
      [ Path.native_exn asm_path ]
      (Path.native_exn out_path);
    ())
;;

let run () =
  let@ env = Eio_main.run in
  let tests_path = Path.(Stdenv.fs env / "../filetests") in
  let workspace_root_path =
    Path.(
      Stdenv.fs env / Ae_path_utils.get_workspace_root_in_build_dir (Stdlib.Sys.getcwd ()))
  in
  let tests =
    Path.read_dir Path.(tests_path)
    |> List.filter ~f:(fun path ->
      Filename.split_extension path |> snd |> [%equal: string option] (Some "c0"))
    |> List.map ~f:(fun path -> Filename.chop_extension path, Path.(tests_path / path))
  in
  List.iter tests ~f:(fun test -> run_test env workspace_root_path tests_path test);
  ()
;;

let%test_unit "filetests" = run ()
