module Caml_unix = Unix
include Core
include Functional
module Vec = Std_vec

let ( let@ ) f x = f x
let todo ?loc () = raise_s [%message "TODO" (loc : Source_code_position.t option)]
let todol loc = raise_s [%message "TODO" (loc : Source_code_position.t)]
