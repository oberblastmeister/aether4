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
  module Temp := Temp_entity.Ident

  module Instr' : sig
    type t =
      { i : Instr.t
      ; index : int
      ; info : Info.t option
      }
    [@@deriving sexp_of]

    val create : ?info:Info.t -> Instr.t -> t
  end

  module Block : sig
    type t [@@deriving sexp_of]

    val instrs : t -> Instr'.t iarray
    val iter_fwd : t -> Instr'.t Iter.t
    val iter_bwd : t -> Instr'.t Iter.t
    val find_jump : t -> Instr'.t
    val of_array : (Instr'.t, [> read ]) Arrayp.t -> t
  end

  module Func : sig
    type t =
      { name : string
      ; blocks : Block.t Label.Map.t
      ; start : Label.t
      ; next_id : Temp_entity.Id.t
      }
    [@@deriving sexp_of]

    val start_block : t -> Block.t
  end

  module Edit : sig
    type t

    val insert : int -> Instr.t -> t
    val remove : int -> t
    val apply : t list -> Block.t -> Block.t
  end
end
