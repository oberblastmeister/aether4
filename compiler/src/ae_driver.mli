open Std

val run_cli : unit -> unit
val compile_source_to_asm : string -> string
val link_files_with_runtime : _ Eio.Process.mgr -> Filename.t list -> Filename.t -> unit
