open Std

let should_trace = ref false
let trace_s s = if !should_trace then print_s s
let set_should_trace b = should_trace := b

let with_trace f =
  let old_should_trace = !should_trace in
  should_trace := true;
  f ();
  should_trace := old_should_trace;
  ()
;;
