open Std
module Path := Eio.Path
module Fs := Eio.Fs

val run_cli : unit -> unit

val compile_path
  :  Eio_unix.Stdenv.base
  -> ?cache_dir_path:Fs.dir_ty Path.t
  -> ?out_path:Fs.dir_ty Path.t
  -> Fs.dir_ty Path.t
  -> unit Or_error.t

val compile_path_to_a_out
  :  Eio_unix.Stdenv.base
  -> ?cache_dir_path:Fs.dir_ty Path.t
  -> Fs.dir_ty Path.t
  -> (Fs.dir_ty Path.t * bool) Or_error.t

val compile_source_to_a_out
  :  Eio_unix.Stdenv.base
  -> ?cache_dir_path:Fs.dir_ty Path.t
  -> string
  -> Fs.dir_ty Path.t
  -> (Fs.dir_ty Path.t * bool) Or_error.t

val compile_source_to_asm : string -> string Or_error.t
val link_files_with_runtime : _ Eio.Process.mgr -> Filename.t list -> Filename.t -> unit
