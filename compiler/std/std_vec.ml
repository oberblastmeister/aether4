open Core
include Dynarray

open struct
  module Iter = Functional.Iter
end

let push = Dynarray.add_last
let iter t ~f = Dynarray.iter f t
let append = Dynarray.append
let append_iter t it = it ~f:(push t)
let append_list = Dynarray.append_list
let to_list = Dynarray.to_list

let rev t =
  let a = Dynarray.to_array t in
  Array.rev_inplace a;
  Dynarray.of_array a
;;
