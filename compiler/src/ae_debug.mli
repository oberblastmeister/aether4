open Std

val debug_s : Sexp.t -> unit [@@alert debug "Debug functions remain in code"]
val debug_ls : Sexp.t Lazy.t -> unit [@@alert debug "Debug functions remain in code"]
val with_debug : (unit -> unit) -> unit [@@alert debug "Debug functions remain in code"]
val set_debug : bool -> unit [@@alert debug "Debug functions remain in code"]
