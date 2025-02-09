open Std
module Id = Ae_entity_id
module Name = Ae_entity_name
module Witness = Ae_entity_witness

module Id_gen = struct
  type 'k t = int ref

  let create ?(start = 0) () = ref start

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

module type S = sig
  module Id : sig
    module Witness : Ae_entity_witness.S

    type t = Witness.t Id.t [@@deriving sexp_of, equal, compare, hash]

    module Table : module type of Id.Table.Make (Witness)
    module Map : module type of Id.Map.Make (Witness)
    module Set : module type of Id.Set.Make (Witness)
  end

  module Name : sig
    type t = Id.Witness.t Name.t [@@deriving sexp_of, equal, compare, hash]

    module Table : module type of Name.Table.Make (Id.Witness)
    module Map : module type of Name.Map.Make (Id.Witness)
    module Set : module type of Name.Set.Make (Id.Witness)
  end
end

module Make_with_witness (Witness : Ae_entity_witness.S) : S = struct
  module Id = struct
    module Witness = Witness

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
    module Table = Id.Table.Make (Witness)
    module Map = Id.Map.Make (Witness)
    module Set = Id.Set.Make (Witness)
  end

  module Name = struct
    module T = struct
      type t = Id.Witness.t Name.t

      let sexp_of_t t = Name.sexp_of_t sexp_of_opaque t
      let t_of_sexp t = Name.t_of_sexp opaque_of_sexp t
      let equal x y = Int.equal x.Name.id y.Name.id
      let compare x y = Int.compare x.Name.id y.Name.id
      let hash_fold_t s x = Int.hash_fold_t s x.Name.id
      let hash x = Int.hash x.Name.id
      let to_int t = (t.Name.id :> int)
    end

    include T
    module Table = Name.Table.Make (Id.Witness)
    module Map = Name.Map.Make (Id.Witness)
    module Set = Name.Set.Make (Witness)
  end
end

module Make () : S = Make_with_witness (struct
    type t [@@deriving sexp_of]
  end)
