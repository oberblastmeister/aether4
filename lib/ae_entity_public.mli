open Std
module Entity_map := Ae_entity_map

module Id : sig
  type 'k t

  val unchecked_of_int : int -> 'a t
  val unchecked_coerce : 'a t -> 'b t

  module Table : Entity_map.S with type 'w Key.t = 'w t
end

module Id_gen : sig
  type 'k t

  val create : ?start:int -> unit -> 'k t
  val next : 'k t -> 'k Id.t
end

module Name : sig
  type 'k t =
    { name : string
    ; id : 'k Id.t
    }

  val create : string -> 'k Id.t -> 'k t
  val unchecked_coerce : 'a t -> 'b t

  module Table : Entity_map.S with type 'w Key.t = 'w t
end

module Intern : sig
  module Name_to_name : sig
    type ('w1, 'w2) t

    val intern : ('w1, 'w2) t -> 'w1 Name.t -> 'w2 Name.t
    val find_exn : ('w1, 'w2) t -> 'w1 Name.t -> 'w2 Name.t
  end
end

module Make () : sig
  module Id : sig
    module Witness : T

    type t = Witness.t Id.t [@@deriving sexp_of, equal, compare, hash]

    module Map : Map.S with type Key.t = t
    module Set : Set.S with type Elt.t = t
  end

  module Name : sig
    type t = Id.Witness.t Name.t [@@deriving sexp_of, equal, compare, hash]

    module Map : Map.S with type Key.t = t
    module Set : Set.S with type Elt.t = t

    module Table : sig
      type 'a t = (Id.Witness.t, 'a) Name.Table.t
    end
  end
end
