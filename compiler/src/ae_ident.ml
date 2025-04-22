open Std
open Ae_intable

open struct
  module Id_table = Ae_id_table
  module Id_gen = Ae_id_gen
end

module type S = sig
  include Intable
  include Comparable.S_plain with type t := t
  include Hashable.S_plain with type t := t
  module Table : Id_table.S with type Key.t = t

  module Id_gen : sig
    type t

    val first_id : t -> int
    val create : int -> t
    val get : t -> int
  end
end

module Make (Id : Intable) = struct
  open struct
    module T = struct
      include Id

      let compare x y = Int.compare (Id.to_int x) (Id.to_int y)
      let hash_fold_t st x = Int.hash_fold_t st (Id.to_int x)
      let hash x = Int.hash (Id.to_int x)
    end
  end

  include Comparable.Make_plain (T)
  include Hashable.Make_plain (T)
  module Table = Id_table.Make (T)
end

module type S_with_name_info_plain = sig
  type t [@@deriving sexp_of]

  include S with type t := t

  val to_string : t -> string
  val create : ?info:Info.t -> string -> int -> t
  val fresh : ?name:string -> ?info:Info.t -> Id_gen.t -> t
  val freshen : Id_gen.t -> t -> t
end

module type S_with_name_info_plain_record = sig
  type t =
    { name : string
    ; id : int
    ; info : Info.t option
    }

  include S_with_name_info_plain with type t := t
end

module Make_with_name_info_plain () : S_with_name_info_plain_record = struct
  module T = struct
    type t =
      { name : string
      ; id : int
      ; info : Info.t option
      }

    let to_int t = t.id
    let to_string t = [%string "%{t.name}@%{t.id#Int}"]
    let sexp_of_t t = Sexp.Atom (to_string t)
  end

  include T
  include Make (T)

  let create ?info name id = { name; id; info }

  module Id_gen = struct
    type t =
      { first_id : int
      ; mutable next_id : int
      }
    [@@deriving sexp_of]

    let create next_id = { first_id = next_id; next_id }
    let first_id t = t.first_id

    let get t =
      let id = t.next_id in
      t.next_id <- id + 1;
      id
    ;;
  end

  let fresh ?(name = "fresh") ?info gen =
    let id = Id_gen.get gen in
    { name; id; info }
  ;;

  let freshen gen t = fresh ~name:t.name ?info:t.info gen
end

module Intern = struct
  module type S = sig
    module Key : sig
      type t
    end

    module Id : S_with_name_info_plain

    type t

    val create : unit -> t
    val intern : t -> Key.t -> Id.t
    val next_id : t -> int

    module Make_global () : sig
      val intern : Key.t -> Id.t
      val next_id : unit -> int
    end
  end

  module Make_string (Id : S_with_name_info_plain) :
    S with module Key = String and module Id = Id = struct
    module Key = String
    module Id = Id

    type t =
      { map : Id.t String.Table.t
      ; mutable next_id : int
      }

    let create () = { map = String.Table.create (); next_id = 0 }
    let next_id t = t.next_id

    let intern (t : t) key =
      Hashtbl.find_or_add t.map key ~default:(fun () ->
        let next_id = t.next_id in
        let id : Id.t = Id.create key next_id in
        t.next_id <- next_id + 1;
        id)
    ;;

    module Make_global () = struct
      let t : t = create ()
      let intern k = intern t k
      let next_id () = next_id t
    end
  end
end

module type S_with_name_info_record = sig
  include S_with_name_info_plain_record
  module Intern : Intern.S with module Key = String and type Id.t = t
end

module Make_with_name_info () : S_with_name_info_record = struct
  module T = struct
    include Make_with_name_info_plain ()
  end

  module Intern = Intern.Make_string (T)
  include T
end
