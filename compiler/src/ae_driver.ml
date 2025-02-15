open Std
module C0 = Ae_c0_std
module Tir = Ae_tir_std
module Lir = Ae_lir_lower_abs_x86
module Abs_x86 = Ae_abs_x86_std
module Flat_x86 = Ae_flat_x86_std
module Args = Ae_args
module F = Filename
module Path_utils = Ae_path_utils

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
  let parts = Filename.parts exe_path in
  let path_before_build, path_after_build =
    List.split_while parts ~f:(fun part -> not (Filename.equal part "_build"))
  in
  match path_after_build with
  | [] -> Filename.dirname exe_path
  | _ ->
    let path_before_build =
      List.fold_left
        (List.tl_exn path_before_build)
        ~init:(List.hd_exn path_before_build)
        ~f:Filename.concat
    in
    Filename.(concat path_before_build "_build/default/runtime/libc0_runtime.a")
;;

let link_files_with_runtime mgr paths out_path =
  let runtime_path = find_runtime_path () in
  Eio.Process.run mgr ([ "zig"; "cc" ] @ [ runtime_path ] @ paths @ [ "-o"; out_path ])
;;

let compile_source_to_asm source =
  let tokens = C0.Lexer.tokenize source in
  let program = C0.Parser.parse tokens |> Result.ok |> Option.value_exn in
  let tir = C0.Lower_tree_ir.lower program in
  let lir = Tir.Lower_lir.lower tir in
  let abs_x86 = Lir.lower lir in
  let alloc = Abs_x86.Regalloc.alloc_func abs_x86 in
  let asm = Abs_x86.Lower_flat_x86.lower alloc abs_x86 in
  let formatted_asm = Flat_x86.Format.format asm in
  formatted_asm
;;

let run_cli () =
  let@ args = Args.with_args in
  let cwd = Stdlib.Sys.getcwd () in
  (* TODO: get the runtime path using this *)
  let path = Caml_unix.readlink "/proc/self/exe" in
  print_endline cwd;
  print_endline path;
  print_s [%sexp (args : Args.t)];
  ()
;;
