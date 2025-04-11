open Std
open Ae_entity_sigs
include Ae_data_bitvec

type nonrec 'w t = t

let sexp_of_t _ t = to_list t |> [%sexp_of: int list]
let t_of_sexp _ t = [%of_sexp: int list] t |> of_list

module Make (Witness : Ae_entity_witness.S) = struct
  type nonrec t = Witness.t t

  let t_of_sexp s = t_of_sexp sexp_of_opaque s
  let sexp_of_t t = sexp_of_t sexp_of_opaque t
end
