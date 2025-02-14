open Std
module Table = Ae_entity_table
module Map = Ae_entity_map
module Set = Ae_entity_set
module Witness = Ae_entity_witness

module Id : sig
  type 'k t = private int

  val unchecked_of_int : int -> 'a t
  val unchecked_coerce : 'a t -> 'b t
  val to_int : 'k t -> int
  val succ : 'a t -> 'a t
  val pred : 'a t -> 'a t

  module Table : Table.S_phantom with type 'w Key.t = 'w t
  module Map : Map.S_phantom with type 'w Key.t = 'w t
  module Set : Set.S_phantom with type 'w Key.t = 'w t
end

module Id_gen : sig
  type 'k t = private int ref

  val create : ?start:int -> unit -> 'k t
  val of_id : 'k Id.t -> 'k t 
  val next : 'k t -> 'k Id.t
end

module Name : sig
  type 'k t =
    { name : string
    ; id : 'k Id.t
    }

  val create : string -> 'k Id.t -> 'k t
  val fresh : ?name:string -> 'k Id_gen.t -> 'k t
  val unchecked_coerce : 'a t -> 'b t

  module Table : Table.S_phantom with type 'w Key.t = 'w t
  module Map : Map.S_phantom with type 'w Key.t = 'w t
  module Set : Set.S_phantom with type 'w Key.t = 'w t
end

module Intern : sig
  module String_to_name : sig
    type 'w t [@@deriving sexp_of]

    val create : unit -> 'w t
    val intern : 'w t -> string -> 'w Name.t
    val find_exn : 'w t -> string -> 'w Name.t

    module Make_global (Witness : T) () : sig
      val intern : string -> Witness.t Name.t
      val find_exn : string -> Witness.t Name.t
    end
  end

  module Name_to_name : sig
    type ('w1, 'w2) t

    val intern : ('w1, 'w2) t -> 'w1 Name.t -> 'w2 Name.t
  end
end

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

module Make_with_witness (_ : Ae_entity_witness.S) : S
module Make () : S
