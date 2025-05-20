open Std
open Ae_trace
module C0 = Ae_c0_std
module Tir = Ae_tir_std
module Lir = Ae_lir_std
module Abs_x86 = Ae_abs_x86_std
module Flat_x86 = Ae_flat_x86_std
module Path_utils = Ae_path_utils
module Spanned = Ae_spanned
module Path = Ae_path
module Fs = Ae_fs
module Process = Ae_process

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
    { cache_dir_path : string option
    ; path : string
    ; emit : Emit.t list
    }

  let create ?cache_dir_path ?(emit = []) path = { cache_dir_path; emit; path }
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
         let path = Core_unix.readlink "/proc/self/exe" in
         self_exe_path := Some path;
         path)
;;

let find_runtime_dir () =
  let exe_path = get_self_exe_path () in
  Filename.dirname exe_path
;;

let find_runtime_path () = Filename.(concat (find_runtime_dir ()) "libc0_runtime.a")

let link_files_with_runtime ~paths ~out_path =
  let runtime_path = find_runtime_path () in
  let args =
    [ "build-exe" ] @ [ runtime_path ] @ paths @ [ "-femit-bin=" ^ out_path; "-fPIE" ]
  in
  let fd_read, fd_write = Spawn.safe_pipe () in
  let child =
    Core_unix.create_process_with_fds
      ~stdin:Generate
      ~stdout:(Use_this fd_write)
      ~stderr:(Use_this fd_write)
      ~prog:"zig"
      ~args
      ~env:(`Extend [])
      ()
  in
  Core_unix.close fd_write;
  Core_unix.close child.stdin;
  (* TODO: don't use wait_exn here *)
  let status = Core_unix.waitpid child.pid in
  match status with
  | Ok () -> ()
  | Error e ->
    let in_channel = Core_unix.in_channel_of_descr fd_read in
    let output = in_channel |> In_channel.input_all in
    raise_s
      [%message
        "Failed to run zig" (e : Core_unix.Exit_or_signal.error) (output : string)]
;;

let compile_source_to_tir ?(emit = []) source =
  let open Result.Let_syntax in
  let tokens = C0.Lexer.tokenize source in
  if Emit.mem emit Tokens
  then print_s [%message "tokens" (tokens : C0.Token.t Spanned.t list)];
  let%bind program =
    Array.of_list tokens
    |> C0.Parser.parse
    |> Result.map_error ~f:(Error.tag_s ~tag:[%message "Parse error"])
  in
  if Emit.mem emit Cst then print_s [%message "cst" (program : C0.Cst.program)];
  let%bind program = C0.Cst_elaborate_ast.elaborate_program program in
  if Emit.mem emit Ast then print_s [%message "ast" (program : C0.Ast.program)];
  let%bind program = C0.Elaborate_types.check_program program in
  let%bind () = C0.Check.check_program program in
  let program = C0.Ast.Closure_convert.convert_program program in
  let tir = C0.Lower_tree_ir.lower_program program in
  if Emit.mem emit Tir_non_ssa then print_s [%message "tir" (tir : Tir.Program.t)];
  let tir =
    (Tir.Program.map_funcs & List.map) ~f:(Tir.Convert_ssa.convert ~renumber:()) tir
  in
  if Emit.mem emit Tir then print_s [%message "tir" (tir : Tir.Program.t)];
  Ok tir
;;

let compile_source_to_asm ?(emit = []) source =
  let open Result.Let_syntax in
  let%bind tir = compile_source_to_tir ~emit source in
  let%bind () = Tir.Check.check_program tir in
  let lir = Tir.Lower_lir.lower_program tir in
  let%bind () = Lir.Check.check_program lir in
  if Emit.mem emit Emit.Lir then print_s [%message (lir : Lir.Program.t)];
  let abs_x86 = Lir.Lower_abs_x86.lower_program lir in
  let%bind () = Abs_x86.Check.check_program abs_x86 in
  if Emit.mem emit Emit.Abs_asm then print_s [%message (abs_x86 : Abs_x86.Program.t)];
  let asm = Abs_x86.Driver.convert_program abs_x86 in
  let formatted_asm = Flat_x86.Format.format asm in
  if Emit.mem emit Emit.Asm
  then begin
    print_endline "asm";
    print_endline formatted_asm
  end;
  Ok formatted_asm
;;

let compile_source_to_a_out (env : Env.t) sources =
  let source, object_files = List.hd_exn sources, List.drop sources 1 in
  List.iter object_files ~f:(fun source ->
    let ext = Filename.split_extension source |> snd |> Option.value_exn in
    assert (String.equal ext "a"));
  let cache_dir_path =
    Option.value env.cache_dir_path ~default:Path.(Sys_unix.getcwd () / ".c0_cache")
  in
  let name = env.path |> Path.basename |> Path.chop_extension in
  let digest = Md5.digest_string (env.path ^ source) in
  let hashed_dir_path =
    let ascii_digest = Md5.to_hex digest in
    let hashed_dir_name =
      Path.basename (Path.chop_extension env.path) ^ "-" ^ ascii_digest
    in
    Path.(cache_dir_path / hashed_dir_name)
  in
  let asm_path = Path.(hashed_dir_path / (name ^ ".s")) in
  let out_path = Path.(hashed_dir_path / "a.out") in
  Fs.create_dir_all ~perm:0o777 hashed_dir_path;
  match compile_source_to_asm ~emit:env.emit source with
  | Ok asm_content ->
    Out_channel.write_all asm_path ~data:asm_content;
    link_files_with_runtime ~paths:(object_files @ [ asm_path ]) ~out_path;
    Ok out_path
  | Error e -> Error e
;;

let compile_path_to_a_out (env : Env.t) =
  let source = In_channel.read_all env.path in
  compile_source_to_a_out env [ source ]
;;

let compile_path ?out_path (env : Env.t) =
  let open Result.Let_syntax in
  let%bind a_out_path = compile_path_to_a_out env in
  let name = env.path |> Filename.basename |> Filename.chop_extension in
  let out_path = Option.value out_path ~default:Path.(Ae_env.getcwd () / name) in
  Fs.copy ~src:a_out_path ~dst:out_path;
  Ok ()
;;

let run_path (env : Env.t) =
  let open Result.Let_syntax in
  let%bind a_out_path = compile_path_to_a_out env in
  let res =
    Core_unix.create_process_with_fds
      ~prog:a_out_path
      ~args:[]
      ~stdin:(Use_this Core_unix.stdin)
      ~stdout:(Use_this Core_unix.stdout)
      ~stderr:(Use_this Core_unix.stderr)
      ()
  in
  Core_unix.waitpid_exn res.pid;
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
       let env = Env.create path in
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
       let env = Env.create path in
       run_path env)
;;

let command =
  Command.group ~summary:"C0 compiler" [ "compile", compile_command; "run", run_command ]
;;

let run_cli () = Command_unix.run command
