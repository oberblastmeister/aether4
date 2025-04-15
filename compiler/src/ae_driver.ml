open Std
open Ae_trace
module C0 = Ae_c0_std
module Tir = Ae_tir_std
module Lir = Ae_lir_std
module Abs_x86 = Ae_abs_x86_std
module Flat_x86 = Ae_flat_x86_std
module Path_utils = Ae_path_utils
module Stdenv = Eio.Stdenv
module Path = Eio.Path
module Process = Eio.Process
module File = Eio.File
module Flow = Eio.Flow
module Fs = Eio.Fs
module Spanned = Ae_spanned

module Emit = struct
  type t =
    | Tokens
    | Cst
    | Ast
    | Tir_non_ssa
    | Tir
    | Lir
    | Abs_asm
    | Asm
  [@@deriving sexp, equal]

  let mem = List.mem ~equal
end

module Env = struct
  type t =
    { env : Eio_unix.Stdenv.base
    ; cache_dir_path : Fs.dir_ty Path.t option
    ; path : Fs.dir_ty Path.t
    ; emit : Emit.t list
    }

  let create ?cache_dir_path ?(emit = []) env path = { env; cache_dir_path; emit; path }
end

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

let compile_source_to_tir ?(emit = []) source =
  let open Result.Let_syntax in
  let tokens = C0.Lexer.tokenize source in
  if Emit.mem emit Tokens
  then print_s [%message "tokens" (tokens : C0.Token.t Spanned.t list)];
  let%bind program =
    C0.Parser.parse tokens
    |> Result.map_error ~f:(Error.tag_s ~tag:[%message "Parse error"])
  in
  if Emit.mem emit Cst then print_s [%message "cst" (program : C0.Cst.program)];
  let%bind program = C0.Cst_elaborate_ast.elaborate_program program in
  if Emit.mem emit Ast then print_s [%message "ast" (program : C0.Ast.program)];
  let%bind program = C0.Elaborate_types.check_program program in
  let%bind () = C0.Check.check_program program in
  let tir = C0.Lower_tree_ir.lower program in
  if Emit.mem emit Tir_non_ssa then print_s [%message "tir_non_ssa" (tir : Tir.Func.t)];
  let tir = Tir.Convert_ssa.convert ~renumber:() tir in
  if Emit.mem emit Tir then print_s [%message "tir" (tir : Tir.Func.t)];
  Ok tir
;;

let compile_source_to_asm ?(emit = []) source =
  let open Result.Let_syntax in
  let%bind tir = compile_source_to_tir ~emit source in
  let%bind () = Tir.Check.check tir in
  let lir = Tir.Lower_lir.lower tir in
  let%bind () = Lir.Check.check lir in
  if Emit.mem emit Emit.Lir then print_s [%message (lir : Lir.Func.t)];
  let abs_x86 = Lir.Lower_abs_x86.lower lir in
  let%bind () = Abs_x86.Check.check abs_x86 in
  if Emit.mem emit Emit.Abs_asm then print_s [%message (abs_x86 : Abs_x86.Func.t)];
  let asm = Abs_x86.Driver.convert abs_x86 in
  let formatted_asm = Flat_x86.Format.format asm in
  if Emit.mem emit Emit.Asm
  then begin
    print_endline "asm";
    print_endline formatted_asm
  end;
  Ok formatted_asm
;;

let copy_file ~src ~dst =
  let@ from_file = Path.with_open_in src in
  let@ to_file = Path.with_open_out ~create:(`Or_truncate 0o777) dst in
  Flow.copy from_file to_file;
  ()
;;

let compile_source_to_a_out (env : Env.t) source =
  let cache_dir_path =
    Option.value env.cache_dir_path ~default:Path.(Stdenv.cwd env.env / ".c0_cache")
  in
  let name = Path.native_exn env.path |> Filename.basename |> Filename.chop_extension in
  let module Digest = Digestif.BLAKE2B in
  let digest = Digest.digestv_string [ Path.native_exn env.path; source ] in
  let hashed_dir_path =
    let ascii_digest =
      Base64.encode_exn ~alphabet:Base64.uri_safe_alphabet (Digest.to_raw_string digest)
    in
    let hashed_dir_name =
      Filename.basename (Filename.chop_extension (Path.native_exn env.path))
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
    match compile_source_to_asm ~emit:env.emit source with
    | Ok asm_content ->
      let@ asm_file = Path.with_open_out ~create:(`Or_truncate 0o777) asm_path in
      Flow.copy_string asm_content asm_file;
      link_files_with_runtime
        (Stdenv.process_mgr env.env)
        [ Path.native_exn asm_path ]
        (Path.native_exn out_path);
      Ok (out_path, true)
    | Error e -> Error e)
;;

let compile_path_to_a_out (env : Env.t) =
  let@ file = Path.with_open_in env.path in
  let source = Flow.read_all file in
  compile_source_to_a_out env source
;;

let compile_path ?out_path (env : Env.t) =
  let open Result.Let_syntax in
  let%bind a_out_path, changed = compile_path_to_a_out env in
  let name = Path.native_exn env.path |> Filename.basename |> Filename.chop_extension in
  let out_path = Option.value out_path ~default:Path.(Stdenv.cwd env.env / name) in
  if Path.is_file out_path
  then if changed then copy_file ~src:a_out_path ~dst:out_path else ()
  else copy_file ~src:a_out_path ~dst:out_path;
  Ok ()
;;

let run_path (env : Env.t) =
  let open Result.Let_syntax in
  let%bind a_out_path, _changed = compile_path_to_a_out env in
  let proc = Stdenv.process_mgr env.env in
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
       let env = Env.create env Path.(Stdenv.fs env / path) in
       (* The path could be a symlink, which could be out of the current working directory *)
       (* This would cause eio to error *)
       (* So bypass this by using Stdenv.fs *)
       compile_path env)
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
       let env = Env.create env Path.(Stdenv.fs env / path) in
       run_path env)
;;

let command =
  Command.group ~summary:"C0 compiler" [ "compile", compile_command; "run", run_command ]
;;

let run_cli () = Command_unix.run command
