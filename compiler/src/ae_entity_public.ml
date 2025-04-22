(* TODO: probably should rewrite this code, it kinda sucks *)
open Std
module Id = Ae_entity_id
module Ident = Ae_entity_ident
module Witness = Ae_entity_witness

module Id_gen = struct
  type 'k t = int ref

  let create ?(start = 0) () = ref start
  let of_id i = ref i

  let next t =
    let id = !t in
    t := id + 1;
    id
  ;;
end

module Table = Ae_entity_table
module Map = Ae_entity_map
module Set = Ae_entity_set
module Intern = Ae_entity_intern
module Bitvec = Ae_entity_bitvec

module type S = sig
  module Witness : Ae_entity_witness.S

  module Id : sig
    type t = Witness.t Id.t [@@deriving sexp_of, equal, compare, hash]

    module Table : module type of Id.Table.Make (Witness)
    module Map : module type of Id.Map.Make (Witness)
    module Set : module type of Id.Set.Make (Witness)
    module Bitvec : module type of Bitvec.Make (Witness)
    include Base.Comparable.S with type t := t
  end

  module Ident : sig
    type t = Witness.t Ident.t [@@deriving sexp_of, equal, compare, hash]

    module Table : module type of Ident.Table.Make (Witness)
    module Map : module type of Ident.Map.Make (Witness)
    module Set : module type of Ident.Set.Make (Witness)
    module Bitvec : module type of Bitvec.Make (Witness)
    include Base.Comparable.S with type t := t
  end
end

module Make_with_witness (Witness : Ae_entity_witness.S) : S = struct
  module Witness = Witness

  module Id = struct
    module T = struct
      type t = Witness.t Id.t

      let sexp_of_t = Int.sexp_of_t
      let t_of_sexp = Int.t_of_sexp
      let equal = Int.equal
      let compare = Int.compare
      let hash_fold_t = Int.hash_fold_t
      let hash = Int.hash
      let to_int (t : t) = (t :> int)
    end

    include T
    include Base.Comparable.Make (T)
    module Table = Id.Table.Make (Witness)
    module Map = Id.Map.Make (Witness)
    module Set = Id.Set.Make (Witness)
    module Bitvec = Bitvec.Make (Witness)
  end

  module Ident = struct
    module T = struct
      type t = Witness.t Ident.t

      let sexp_of_t t = Ident.sexp_of_t sexp_of_opaque t
      let t_of_sexp t = Ident.t_of_sexp opaque_of_sexp t
      let equal x y = Int.equal x.Ident.id y.Ident.id
      let compare x y = Int.compare x.Ident.id y.Ident.id
      let hash_fold_t s x = Int.hash_fold_t s x.Ident.id
      let hash x = Int.hash x.Ident.id
      let to_int t = (t.Ident.id :> int)
    end

    include T
    include Base.Comparable.Make (T)
    module Table = Ident.Table.Make (Witness)
    module Map = Ident.Map.Make (Witness)
    module Set = Ident.Set.Make (Witness)
    module Bitvec = Bitvec.Make (Witness)
  end
end

module Make () : S = Make_with_witness (struct
    type t [@@deriving sexp_of]
  end)
