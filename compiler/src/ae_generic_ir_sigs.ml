open Std

open struct
  module Entity = Ae_entity_std
  module Label_entity = Ae_label_entity
  module Label = Label_entity.Ident
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

    (* Is this a control flow instruction?
      If so give us the labels it jumps to *)
    val jumps : t -> Label.t list option
  end

  module Func_data : sig
    type t [@@deriving sexp_of]
  end
end

module type Ir = sig
  module Arg : Arg
  open Arg
  module Temp_entity := Arg.Temp_entity
  module Temp := Temp_entity.Ident

  module Instr' : sig
    type t =
      { i : Instr.t
      ; index : int
      ; info : Info.t option
      }
    [@@deriving sexp_of]

    val create_unindexed : ?info:Info.t -> Instr.t -> t

    module Table : Entity.Table.S with type 'w Key.t = t
    module Map : Entity.Map.S with type 'w Key.t = t
  end

  module Block : sig
    type t =
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
    val succ_table : t -> Label.t list Label.Table.t
  end

  module Edit : sig
    type t

    val insert : Instr'.t -> t
    val remove : Instr'.t -> t
    val apply : t list -> Block.t -> Block.t
  end
end
