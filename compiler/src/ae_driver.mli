open Std
module Tir := Ae_tir_std
module Path = Ae_path

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
    { cache_dir_path : string option
    ; path : string
    ; emit : Emit.t list
    }

  val create : ?cache_dir_path:Path.t -> ?emit:Emit.t list -> Path.t -> t
end

val find_runtime_dir : unit -> string
val run_cli : unit -> unit
val compile_path : ?out_path:Path.t -> Env.t -> unit Or_error.t
val compile_path_to_a_out : Env.t -> Path.t Or_error.t
val compile_source_to_tir : ?emit:Emit.t list -> string -> Tir.Program.t Or_error.t
val compile_source_to_a_out : Env.t -> string list -> Path.t Or_error.t
val compile_source_to_asm : ?emit:Emit.t list -> string -> string Or_error.t
val link_files_with_runtime : paths:Filename.t list -> out_path:Filename.t -> unit
