(* TODO: fix this file *)
open Std

(* open struct
  module Label_entity = Ae_label_entity
  module Label = Label_entity.Ident
  module Graph = Ae_data_graph_std
end

module type Ir = sig
  module Instr : sig
    type t [@@deriving sexp_of]
  end

  module Block : sig
    type t [@@deriving sexp_of]

    val iter_instrs_forward : t -> Instr.t Iter.t
    val iter_instrs_backward : t -> Instr.t Iter.t
  end
end

module Direction = struct
  type t =
    | Forward
    | Backward
end

module type S = sig
  module Ir : Ir
  open Ir

  module Instr_transfer : sig
    type 'd t =
      { transfer : Instr.t -> 'd -> 'd
      ; changed : current_fact:'d -> new_fact:'d -> bool
      ; empty : 'd
      ; combine : 'd list -> 'd
      ; direction : Direction.t
      ; sexp_of_domain : 'd -> Sexp.t
      }
  end

  module Block_transfer : sig
    type 'd t =
      { transfer : Label.t -> Block.t -> other_facts:'d -> current_fact:'d -> 'd option
      ; combine : 'd list -> 'd
      ; empty : 'd
      ; direction : Direction.t
      ; sexp_of_domain : 'd -> Sexp.t
      }
  end

  val instr_to_block_transfer : 'd Instr_transfer.t -> 'd Block_transfer.t

  val run_block_transfer
    :  'd Block_transfer.t
    -> Block.t Graph.t
    -> 'd Label.Table.t * 'd Label.Table.t
end

module Make (Ir : Ir) = struct
  module Ir = Ir
  open Ir

  module Instr_transfer = struct
    type 'd t =
      { transfer : Instr.t -> 'd -> 'd
      ; changed : current_fact:'d -> new_fact:'d -> bool
      ; empty : 'd
      ; combine : 'd list -> 'd
      ; direction : Direction.t
      ; sexp_of_domain : 'd -> Sexp.t
      }
  end

  module Block_transfer = struct
    type 'd t =
      { transfer : Label.t -> Block.t -> other_facts:'d -> current_fact:'d -> 'd option
      ; combine : 'd list -> 'd
      ; empty : 'd
      ; direction : Direction.t
      ; sexp_of_domain : 'd -> Sexp.t
      }
  end

  let instr_to_block_transfer _ = todo ()
  let run_block_transfer _ = todo ()
end *)
