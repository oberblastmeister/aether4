open Std

open struct
  module BV = CCBV
end

type t = BV.t

let create ?(size = 0) ?(default = false) () = BV.create ~size default
let add = BV.set
let remove t i = BV.set_bool t i false
let mem t i = BV.get t i
let of_list = BV.of_list
let to_list = BV.to_list
let[@inline] iter t ~f = BV.iter_true t f
let of_iter it = BV.of_iter (Iter.to_fn it)
let sexp_of_t t = to_list t |> [%sexp_of: int list]
let t_of_sexp t = [%of_sexp: int list] t |> of_list
