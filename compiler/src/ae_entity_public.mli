open Std
module Table = Ae_entity_table
module Map = Ae_entity_map
module Set = Ae_entity_set
module Witness = Ae_entity_witness

module Id : sig
  type 'k t = private int [@@deriving sexp]

  val unchecked_of_int : int -> 'a t
  val unchecked_coerce : 'a t -> 'b t
  val to_int : 'k t -> int
  val succ : 'a t -> 'a t
  val offset : 'a t -> int -> 'a t
  val pred : 'a t -> 'a t
  val to_string : 'a t -> string

  module Table : Table.S_phantom with type 'w Key.t = 'w t
  module Map : Map.S_phantom with type 'w Key.t = 'w t
  module Set : Set.S_phantom with type 'w Key.t = 'w t

  val ( > ) : 'k t -> 'k t -> bool
  val ( < ) : 'k t -> 'k t -> bool
  val ( <= ) : 'k t -> 'k t -> bool
  val ( >= ) : 'k t -> 'k t -> bool
  val ( = ) : 'k t -> 'k t -> bool
  val add_table : 'a -> ('w, 'a) Table.t -> 'w t
end

module Bitvec : sig
  type 'w t [@@deriving sexp]

  val create : ?size:int -> ?default:bool -> unit -> 'w t
  val add : 'a t -> 'a Id.t -> unit
  val remove : 'a t -> 'a Id.t -> unit
  val of_list : 'a Id.t list -> 'a t
  val mem : 'a t -> 'a Id.t -> bool
  val to_list : 'a t -> 'a Id.t list

  module Make (Witness : Ae_entity_witness.S) : sig
    type nonrec t = Witness.t t [@@deriving sexp]
  end
end

module Id_gen : sig
  type 'k t = private int ref

  val create : ?start:int -> unit -> 'k t
  val of_id : 'k Id.t -> 'k t
  val next : 'k t -> 'k Id.t
end

module Ident : sig
  type 'k t = private
    { name : string
    ; id : 'k Id.t
    ; info : Info.t option
    }
  [@@deriving sexp]

  val create : ?info:Info.t -> string -> 'k Id.t -> 'k t
  val to_id : 'k t -> 'k Id.t
  val to_int : 'k t -> int
  val fresh : ?name:string -> ?info:Info.t -> 'k Id_gen.t -> 'k t
  val freshen : 'k Id_gen.t -> 'k t -> 'k t
  val unchecked_coerce : 'a t -> 'b t

  module Table : Table.S_phantom with type 'w Key.t = 'w t
  module Map : Map.S_phantom with type 'w Key.t = 'w t
  module Set : Set.S_phantom with type 'w Key.t = 'w t

  val add_table : ?name:string -> ?info:Info.t -> 'a -> ('w, 'a) Table.t -> 'w t
end

module Intern : sig
  module String_to_name : sig
    type 'w t [@@deriving sexp_of]

    val create : unit -> 'w t
    val intern : 'w t -> string -> 'w Ident.t
    val fresh : ?name:string -> 'w t -> 'w Ident.t
    val find_exn : 'w t -> string -> 'w Ident.t
    val next_id : 'w t -> 'w Id.t

    module Make_global (Witness : T) () : sig
      val intern : string -> Witness.t Ident.t
      val fresh : ?name:string -> unit -> 'w Ident.t
      val find_exn : string -> Witness.t Ident.t
      val next_id : unit -> 'w Id.t
    end
  end

  module Ident_to_name : sig
    type ('w1, 'w2) t

    val intern : ('w1, 'w2) t -> 'w1 Ident.t -> 'w2 Ident.t
  end
end

module type S = sig
  module Witness : Ae_entity_witness.S

  module Id : sig
    type t = Witness.t Id.t [@@deriving sexp_of, equal, compare, hash]

    (* TODO: remove this shitty code *)
    module Table : module type of struct
      include Id.Table.Make (Witness)
    end

    module Map : module type of struct
      include Id.Map.Make (Witness)
    end

    module Set : module type of struct
      include Id.Set.Make (Witness)
    end

    module Bitvec : module type of struct
      include Bitvec.Make (Witness)
    end

    include Base.Comparable.S with type t := t
  end

  module Ident : sig
    type t = Witness.t Ident.t [@@deriving sexp_of, equal, compare, hash]

    module Table : module type of struct
      include Ident.Table.Make (Witness)
    end

    module Map : module type of struct
      include Ident.Map.Make (Witness)
    end

    module Set : module type of struct
      include Ident.Set.Make (Witness)
    end

    module Bitvec : module type of struct
      include Bitvec.Make (Witness)
    end

    include Base.Comparable.S with type t := t
  end
end

module Make_with_witness (_ : Ae_entity_witness.S) : S
module Make () : S
