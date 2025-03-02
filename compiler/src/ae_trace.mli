open Std

val trace_s : Sexp.t -> unit [@@alert debug "Trace functions remain in code"]
val trace_ls : Sexp.t Lazy.t -> unit [@@alert trace "Trace functions remain in code"]
val with_trace : (unit -> unit) -> unit [@@alert trace "Trace functions remain in code"]
val set_trace : bool -> unit [@@alert trace "Trace functions remain in code"]
