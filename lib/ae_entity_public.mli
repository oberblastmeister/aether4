open Std
module Table = Ae_entity_table
module Map = Ae_entity_map

module Id : sig
  type 'k t

  val unchecked_of_int : int -> 'a t
  val unchecked_coerce : 'a t -> 'b t

  module Table : Table.S with type 'w Key.t = 'w t
  module Map : Map.S with type 'w Key.t = 'w t
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

  module Table : Table.S with type 'w Key.t = 'w t
  module Map : Map.S with type 'w Key.t = 'w t
end

module type S = sig
  module Id : sig
    module Witness : Ae_entity_witness.S

    type t = Witness.t Id.t [@@deriving sexp_of, equal, compare, hash]

    module Table : module type of Name.Table.Make (Witness)
    module Map : module type of Name.Map.Make (Witness)
  end

  module Name : sig
    type t = Id.Witness.t Name.t [@@deriving sexp_of, equal, compare, hash]

    module Table : module type of Name.Table.Make (Id.Witness)
    module Map : module type of Name.Map.Make (Id.Witness)
  end
end

module Make_with_witness (_ : Ae_entity_witness.S) : S
module Make () : S
