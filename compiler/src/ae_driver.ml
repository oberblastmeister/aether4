open Std
module C0 = Ae_c0_std
module Tir = Ae_tir_std
module Lir = Ae_lir_lower_abs_x86
module Abs_x86 = Ae_abs_x86_std
module Flat_x86 = Ae_flat_x86_std
module F = Filename
module Path_utils = Ae_path_utils
module Stdenv = Eio.Stdenv
module Path = Eio.Path
module Process = Eio.Process
module File = Eio.File
module Flow = Eio.Flow

let get_self_exe_path =
  let lock = Stdlib.Mutex.create () in
  let self_exe_path = ref None in
  fun () ->
    match !self_exe_path with
    | Some path -> path
    | None ->
      let@ () = Stdlib.Mutex.protect lock in
      (match !self_exe_path with
       | Some path -> path
       | None ->
         let path = Caml_unix.readlink "/proc/self/exe" in
         self_exe_path := Some path;
         path)
;;

let find_runtime_path () =
  let exe_path = get_self_exe_path () in
  Filename.(concat (Filename.dirname exe_path) "libc0_runtime.a")
;;

let link_files_with_runtime mgr paths out_path =
  let runtime_path = find_runtime_path () in
  Eio.Process.run
    mgr
    ([ "zig"; "cc"; "-Wno-unused-command-line-argument" ]
     @ [ runtime_path ]
     @ paths
     @ [ "-o"; out_path ])
;;

let compile_source_to_asm source =
  let open Result.Let_syntax in
  let tokens = C0.Lexer.tokenize source in
  let%bind program =
    C0.Parser.parse tokens |> Result.map_error ~f:(function Sexp s -> Error.create_s s)
  in
  let%bind () = C0.Check.check_program program in
  let tir = C0.Lower_tree_ir.lower program in
  let lir = Tir.Lower_lir.lower tir in
  let abs_x86 = Lir.lower lir in
  let alloc = Abs_x86.Regalloc.alloc_func abs_x86 in
  let asm = Abs_x86.Lower_flat_x86.lower alloc abs_x86 in
  let formatted_asm = Flat_x86.Format.format asm in
  Ok formatted_asm
;;

let copy_file ~src ~dst =
  let@ from_file = Path.with_open_in src in
  let@ to_file = Path.with_open_out ~create:(`Or_truncate 0o777) dst in
  Flow.copy from_file to_file;
  ()
;;

let compile_source_to_a_out env ?cache_dir_path source path =
  let cache_dir_path =
    Option.value cache_dir_path ~default:Path.(Stdenv.cwd env / ".c0_cache")
  in
  let name = Path.native_exn path |> Filename.basename |> Filename.chop_extension in
  let module Digest = Digestif.BLAKE2B in
  let digest = Digest.digestv_string [ Path.native_exn path; source ] in
  let hashed_dir_path =
    let ascii_digest =
      Base64.encode_exn ~alphabet:Base64.uri_safe_alphabet (Digest.to_raw_string digest)
    in
    let hashed_dir_name =
      Filename.basename (Filename.chop_extension (Path.native_exn path))
      ^ "-"
      ^ ascii_digest
    in
    Path.(cache_dir_path / hashed_dir_name)
  in
  let asm_path = Path.(hashed_dir_path / (name ^ ".s")) in
  let out_path = Path.(hashed_dir_path / "a.out") in
  Path.mkdirs ~exists_ok:true ~perm:0o777 hashed_dir_path;
  if Path.is_file out_path
  then Ok (out_path, false)
  else (
    match compile_source_to_asm source with
    | Ok asm_content ->
      let@ asm_file = Path.with_open_out ~create:(`Or_truncate 0o777) asm_path in
      Flow.copy_string asm_content asm_file;
      link_files_with_runtime
        (Stdenv.process_mgr env)
        [ Path.native_exn asm_path ]
        (Path.native_exn out_path);
      Ok (out_path, true)
    | Error e -> Error e)
;;

let compile_path_to_a_out env ?cache_dir_path path =
  let@ file = Path.with_open_in path in
  let source = Flow.read_all file in
  compile_source_to_a_out env ?cache_dir_path source path
;;

let compile_path env ?cache_dir_path ?out_path path =
  let open Result.Let_syntax in
  let%bind a_out_path, changed = compile_path_to_a_out env ?cache_dir_path path in
  let name = Path.native_exn path |> Filename.basename |> Filename.chop_extension in
  let out_path = Option.value out_path ~default:Path.(Stdenv.cwd env / name) in
  if Path.is_file out_path
  then if changed then copy_file ~src:a_out_path ~dst:out_path else ()
  else copy_file ~src:a_out_path ~dst:out_path;
  Ok ()
;;

let run_path env ?cache_dir_path path =
  let open Result.Let_syntax in
  let%bind a_out_path, _changed = compile_path_to_a_out env ?cache_dir_path path in
  let proc = Stdenv.process_mgr env in
  Process.run proc [ Path.native_exn a_out_path ];
  Ok ()
;;

let compile_command =
  Command.basic_or_error
    ~summary:"Compile a c0 file"
    ~readme:(fun () -> "More detailed information")
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map verbose = flag "-v" no_arg ~doc:"more logging output"
     and path = anon ("filename" %: Filename_unix.arg_type) in
     fun () ->
       if not (Stdlib.Sys.is_regular_file path)
       then raise_s [%message "Path does not exist!" (path : string)];
       let@ env = Eio_main.run in
       (* The path could be a symlink, which could be out of the current working directory *)
       (* This would cause eio to error *)
       (* So bypass this by using Stdenv.fs *)
       compile_path env Path.(Stdenv.fs env / path))
;;

let run_command =
  Command.basic_or_error
    ~summary:"Run a c0 file"
    ~readme:(fun () -> "More detailed information")
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map path = anon ("filename" %: Filename_unix.arg_type) in
     fun () ->
       if not (Stdlib.Sys.is_regular_file path)
       then raise_s [%message "Path does not exist!" (path : string)];
       let@ env = Eio_main.run in
       run_path env Path.(Stdenv.fs env / path))
;;

let command =
  Command.group ~summary:"C0 compiler" [ "compile", compile_command; "run", run_command ]
;;

let run_cli () = Command_unix.run command
