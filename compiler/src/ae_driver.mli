open Std
module Tir := Ae_tir_std
module Path := Eio.Path
module Fs := Eio.Fs

module Emit : sig
  type t =
    | Tokens
    | Cst
    | Ast
    | Tir_non_ssa
    | Tir
    | Lir
    | Abs_asm
    | Asm
  [@@deriving sexp]
end

module Env : sig
  type t =
    { env : Eio_unix.Stdenv.base
    ; cache_dir_path : Fs.dir_ty Path.t option
    ; path : Fs.dir_ty Path.t
    ; emit : Emit.t list
    }

  val create
    :  ?cache_dir_path:Fs.dir_ty Path.t
    -> ?emit:Emit.t list
    -> Eio_linux.stdenv
    -> Fs.dir_ty Path.t
    -> t
end

val run_cli : unit -> unit
val compile_path : ?out_path:Fs.dir_ty Path.t -> Env.t -> unit Or_error.t
val compile_path_to_a_out : Env.t -> (Fs.dir_ty Path.t * bool) Or_error.t
val compile_source_to_tir : ?emit:Emit.t list -> string -> Tir.Func.t Or_error.t
val compile_source_to_a_out : Env.t -> string -> (Fs.dir_ty Path.t * bool) Or_error.t
val compile_source_to_asm : ?emit:Emit.t list -> string -> string Or_error.t
val link_files_with_runtime : _ Eio.Process.mgr -> Filename.t list -> Filename.t -> unit
