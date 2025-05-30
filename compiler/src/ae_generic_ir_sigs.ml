open Std

open struct
  module Entity = Ae_entity_std
  module Label_entity = Ae_label_entity
  module Stack_slot_entity = Ae_stack_slot_entity
  module Label = Label_entity.Ident
  module Dominators = Ae_dominators
  module Graph = Ae_data_graph_std
end

module type Instr_S = sig
  module Temp_entity : Entity.S
  module Temp : module type of Temp_entity.Ident

  module Location : sig
    type t [@@deriving sexp_of]

    val temp_val : t -> Temp.t option
    val of_temp : Temp.t -> t
  end

  module Ty : sig
    type t [@@deriving sexp_of, equal, compare]
  end

  module Block_param : sig
    type t =
      { param : Location.t
      ; ty : Ty.t
      }
    [@@deriving sexp_of, fields ~fields ~getters ~iterators:create]
  end

  module Block_call : sig
    type t =
      { label : Label.t
      ; args : Location.t list
      }
    [@@deriving sexp_of, fields ~fields ~getters ~iterators:create]
  end

  module Ann : sig
    type t [@@deriving sexp_of]

    val default : t
  end

  module Instr : sig
    type t [@@deriving sexp_of]

    val nop : t
    val is_nop : t -> bool
    val block_params : Block_param.t list -> t
    val is_block_params : t -> bool
    val block_params_val : t -> Block_param.t list option
    val jump : Block_call.t -> t
    val jump_val : t -> Block_call.t option
    val is_jump : t -> bool
    val is_control : t -> bool

    (*
       Sometimes the type of the use is not directly known just by inspecting the instruction alone.
      This function only needs to iterate over the *known* types alone with the associated temporary.
      This is as opposed to definitions which always have a known type by inspecting the instruction alone.
      For example, for block calls we have to inspect the block parameters of the destination label.
      For function calls we need to inspect the parameters in function definition.
    *)
    val iter_uses_with_known_ty : t -> (Temp.t * Ty.t) Iter.t
    val iter_uses : t -> Temp.t Iter.t
    val iter_defs : t -> Temp.t Iter.t
    val iter_defs_with_ty : t -> (Temp.t * Ty.t) Iter.t
    val map_uses : t -> f:(Temp.t -> Temp.t) -> t
    val map_defs : t -> f:(Temp.t -> Temp.t) -> t
    val map_block_calls : t -> f:(Block_call.t -> Block_call.t) -> t
    val iter_block_calls : t -> Block_call.t Iter.t
  end
end

module type Ir_simple = sig
  include Instr_S

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

    module Table : Entity.Table.S with type 'w Key.t = t
    module Map : Entity.Map.S with type 'w Key.t = t
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

    module Table : Entity.Table.S with type 'w Key.t = t
    module Map : Entity.Map.S with type 'w Key.t = t
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

    val create : unit -> t
    val add_insert : t -> Label.t -> Instr'.t -> unit
    val add_inserts : t -> Label.t -> Instr'.t list -> unit
    val add_remove : t -> Label.t -> Instr'.t -> unit
    val add_replace : t -> Label.t -> Instr'.t -> unit
    val add_edits : t -> Label.t -> Edit.t list -> unit
    val apply_blocks : ?no_sort:unit -> t -> Block.t Label.Map.t -> Block.t Label.Map.t
  end

  module Func : sig
    type t [@@deriving sexp_of]

    val start : t -> Label.t
    val blocks : t -> Block.t Label.Map.t
    val set_blocks : t -> Block.t Label.Map.t -> t
    val apply_multi_edit : ?no_sort:unit -> Multi_edit.t -> t -> t
    val create_temp_gen : t -> Temp_entity.Witness.t Entity.Id_gen.t
    val apply_temp_gen : Temp_entity.Witness.t Entity.Id_gen.t -> t -> t
    val create_label_gen : t -> Label_entity.Witness.t Entity.Id_gen.t
    val apply_label_gen : Label_entity.Witness.t Entity.Id_gen.t -> t -> t
    val start_block : t -> Block.t
    val succ_map : t -> Adj_map.t
    val iter_blocks : t -> Block.t Iter.t
    val find_block_exn : t -> Label.t -> Block.t
    val pred_table : t -> Adj_table.t
    val pred_table_of_succ : Adj_table.t -> Adj_table.t
    val succ_table : t -> Label.t list Label.Table.t
    val bi_graph : t -> Label.t Graph.Bi.t
    val graph : t -> Label.t Graph.t
    val compute_idoms : ?graph:Label.t Graph.Bi.t -> t -> Dominators.t
    val compute_dom_tree : ?graph:Label.t Graph.Bi.t -> t -> Dominators.Tree.t
    val labels_postorder : t -> Label.t Vec.t
    val labels_reverse_postorder : t -> Label.t Vec.t
  end
end

(* using make ir_extnsion *)
module type Ir = sig
  include Ir_simple
  module Temp := Temp_entity.Ident

  module Func : sig
    include module type of Func

    val get_ty_table : t -> Ty.t Temp.Table.t
  end
end
