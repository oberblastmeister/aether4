open Std

open struct
  module Entity = Ae_entity_std
  module Label_entity = Ae_label_entity
  module Label = Label_entity.Ident
  module Block_call = Ae_block_call
  module Dominators = Ae_dominators
  module Graph = Ae_data_graph_std
end

module type Arg = sig
  module Temp_entity : Entity.S
  module Temp := Temp_entity.Ident

  module Instr : sig
    type t [@@deriving sexp_of]

    val nop : t
    val is_nop : t -> bool
    val iter_uses : t -> Temp.t Iter.t
    val iter_defs : t -> Temp.t Iter.t
    val map_uses : t -> f:(Temp.t -> Temp.t) -> t
    val map_defs : t -> f:(Temp.t -> Temp.t) -> t

    (* Is this a control flow instruction?
      If so give us the labels it jumps to *)
    val jumps : t -> Block_call.Make(Temp_entity).t list option
  end

  module Func_data : sig
    type t [@@deriving sexp_of]
  end
end

module type Ir = sig
  module Arg : Arg
  open Arg

  module Instr_ext : sig
    val jumps_labels : Instr.t -> Label.t list option
  end

  module Temp := Temp_entity.Ident

  module Instr' : sig
    type t =
      { i : Instr.t
      ; index : int
      ; info : Info.t option
      }
    [@@deriving sexp_of]

    val map : t -> f:(Instr.t -> Instr.t) -> t
    val create_unindexed : ?info:Info.t -> Instr.t -> t

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
    val find_jump : t -> Instr'.t
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

  module Func : sig
    type t =
      { name : string
      ; blocks : Block.t Label.Map.t
      ; start : Label.t
      ; next_temp_id : Temp_entity.Id.t
      ; next_label_id : Label_entity.Id.t
      }
    [@@deriving sexp_of]

    val start_block : t -> Block.t
    val succ_map : t -> Adj_map.t
    val iter_blocks : t -> Block.t Iter.t
    val pred_table : t -> Adj_table.t
    val pred_table_of_succ : Adj_table.t -> Adj_table.t
    val succ_table : t -> Label.t list Label.Table.t
    val compute_idoms : ?graph:Label.t Graph.Bi.t -> t -> Dominators.Immediate.t
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
    val add_remove : t -> Label.t -> Instr'.t -> unit
    val add_replace : t -> Label.t -> Instr'.t -> unit
    val add_edits : t -> Label.t -> Edit.t list -> unit
    val apply_blocks : ?no_sort:unit -> t -> Block.t Label.Map.t -> Block.t Label.Map.t
  end

  module Std : sig
    module Instr : sig
      include module type of struct
        include Instr
      end

      include module type of struct
        include Instr_ext
      end
    end

    module Instr' = Instr'
    module Block = Block
    module Adj_map = Adj_map
    module Adj_table = Adj_table
    module Func = Func
    module Edit = Edit
    module Temp_entity = Arg.Temp_entity
    module Temp = Temp_entity.Ident
  end
end
