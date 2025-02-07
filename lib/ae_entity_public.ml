open Std
module Id = Ae_entity_id
module Structures = Ae_structures

module Id_gen = struct
  type 'k t = int ref

  let create ?(start = 0) () = ref start

  let next t =
    let id = !t in
    t := id + 1;
    id
  ;;
end

module Name = Ae_entity_name
module Entity_map = Ae_entity_map
module Intern = Ae_entity_intern

module Make () = struct
  module Id = struct
    module T = struct
      module Witness = struct
        type t [@@deriving sexp, compare, hash, equal]
      end

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
    module Map = Map.Make (T)
    module Set = Set.Make (T)
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
    module Map = Map.Make (T)
    module Set = Set.Make (T)

    module Table = struct
      type 'a t = (Id.Witness.t, 'a) Name.Table.t
    end
  end
end

module Map = Entity_map
