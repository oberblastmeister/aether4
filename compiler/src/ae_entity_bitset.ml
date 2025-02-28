open Std
open Ae_entity_sigs

open struct
  module BV = CCBV
end

type 'w t = BV.t

let create ?(size = 0) ?(default = false) () = BV.create ~size default
let add = BV.set
let remove t i = BV.set_bool t i false
let mem t i = BV.get t i

let of_list l =
  let t = create () in
  List.iter l ~f:(add t);
  t
;;

let to_list = BV.to_list
let sexp_of_t _ t = to_list t |> [%sexp_of: int list]
let t_of_sexp _ t = [%of_sexp: int list] t |> of_list

module Make (Witness : Ae_entity_witness.S) = struct
  type nonrec t = Witness.t t

  let t_of_sexp s = t_of_sexp sexp_of_opaque s
  let sexp_of_t t = sexp_of_t sexp_of_opaque t
end
