open Std

let should_debug = ref false
let debug_s s = if !should_debug then print_s s
let debug_ls ls = if !should_debug then print_s (Lazy.force ls)
let set_debug b = should_debug := b

let with_debug f =
  let old_should_trace = !should_debug in
  should_debug := true;
  f ();
  should_debug := old_should_trace;
  ()
;;
