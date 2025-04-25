(*
   TODO: don't make the Func directly inside ae_generic_ir_make_intf,
   have a function that makes the cfg utilities like iter_blocks, labels_postorder, compute_dom_tree, etc.
   Let the users make the funcs themselves.
*)
open Std

open struct
  module Label = Ae_label
  module Dominators = Ae_dominators
  module Graph = Ae_data_graph_std
end

module type Arg = Ae_generic_ir_sigs.Instr_S

module type S = sig
  module Arg : Arg
  open Arg

  module Instr' : sig
    type t =
      { i : Instr.t
      ; index : int
      ; info : Info.t option
      ; ann : Ann.t
      }
    [@@deriving sexp_of]

    val instr : t -> Instr.t
    val map : t -> f:(Instr.t -> Instr.t) -> t
    val create : ?ann:Ann.t -> ?info:Info.t -> Instr.t -> int -> t
    val create_unindexed : ?ann:Ann.t -> ?info:Info.t -> Instr.t -> t

    (* module Table : Entity.Table.S with type 'w Key.t = t
    module Map : Entity.Map.S with type 'w Key.t = t *)
  end

  module Block : sig
    (* private so that we make sure to get the indices correct *)
    type t = private
      { label : Label.t
      ; body : Instr'.t iarray
      }
    [@@deriving sexp_of]

    val instrs : t -> Instr'.t iarray
    val iter_fwd : t -> Instr'.t Iter.t
    val iter_bwd : t -> Instr'.t Iter.t
    val find_control : t -> Instr'.t
    val find_block_params : t -> Instr'.t
    val create : Label.t -> (Instr'.t, [> read ]) Arrayp.t -> t
    val create_id : Label.t -> Instr'.t iarray -> t
    val get_succ : t -> Label.t list

    (* module Table : Entity.Table.S with type 'w Key.t = t
    module Map : Entity.Map.S with type 'w Key.t = t *)
  end

  module Adj_map : sig
    type t = Label.t list Label.Map.t
  end

  module Adj_table : sig
    type t = Label.t list Label.Table.t
  end

  module Edit : sig
    type t [@@deriving sexp_of]

    val insert : Instr'.t -> t
    val remove : Instr'.t -> t
    val apply : ?no_sort:unit -> t list -> Block.t -> Block.t
  end

  module Multi_edit : sig
    type t [@@deriving sexp_of]

    (*
       very important! make sure when creating the multi edit you set ~rev:() if you are iterating backwards for each block using iter_rev
    *)
    val create : ?rev:unit -> unit -> t
    val add_insert : t -> Label.t -> Instr'.t -> unit
    val add_inserts : t -> Label.t -> Instr'.t list -> unit
    val add_remove : t -> Label.t -> Instr'.t -> unit
    val add_replace : t -> Label.t -> Instr'.t -> unit
    val add_edits : t -> Label.t -> Edit.t list -> unit
    val apply_blocks : ?no_sort:unit -> t -> Block.t Label.Map.t -> Block.t Label.Map.t
  end
end
