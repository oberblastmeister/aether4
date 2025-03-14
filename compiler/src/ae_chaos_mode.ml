module Spill_mode = struct
  type t =
    | All
    | Random
  [@@deriving sexp, equal, compare]
end

module Options = struct
  type t = { spill_mode : Spill_mode.t option }

  let default = { spill_mode = None }
end

let options = ref Options.default

let with_options update_options f =
  let old_options = !options in
  options := update_options !options;
  let res = f () in
  options := old_options;
  res
;;

let get_options () = !options
