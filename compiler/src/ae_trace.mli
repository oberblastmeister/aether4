open Std

val trace_s : Sexp.t -> unit [@@alert trace "Trace functions remain in code"]
val with_trace : (unit -> unit) -> unit [@@alert trace "Trace functions remain in code"]
