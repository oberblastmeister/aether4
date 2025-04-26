open Std

let should_trace = ref false
let trace_s s = if !should_trace then print_s s
let trace_ls ls = if !should_trace then print_s (Lazy.force ls)
let trace_endline s = if !should_trace then print_endline s
let set_trace b = should_trace := b
let get_trace () = !should_trace

let with_trace b f =
  let old_should_trace = !should_trace in
  should_trace := b;
  let res = f () in
  should_trace := old_should_trace;
  res
;;
