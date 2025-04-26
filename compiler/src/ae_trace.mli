open Std

val trace_s : Sexp.t -> unit [@@alert debug "Trace functions remain in code"]
val trace_ls : Sexp.t Lazy.t -> unit [@@alert trace "Trace functions remain in code"]
val trace_endline : string -> unit [@@alert trace "Trace functions remain in code"]
val with_trace : bool -> (unit -> 'a) -> 'a
val set_trace : bool -> unit
val get_trace : unit -> bool
