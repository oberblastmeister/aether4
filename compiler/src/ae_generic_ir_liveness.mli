open Std
open Ae_generic_ir_import

module Make (Ir : Ir) : sig
  open Make_std(Ir)

  module Live_set : sig
    type t [@@deriving sexp_of]

    val find : t -> Label.t -> Temp.Set.t
  end

  module Next_use_table : sig
    type t [@@deriving sexp_of]

    val find : t -> Label.t -> int Temp.Map.t
  end

  val backwards_transfer : Instr.t -> Temp.Set.t -> Temp.Set.t
  val next_use_backwards_transfer : Instr.t -> int Temp.Map.t -> int Temp.Map.t
  val compute_def_blocks_non_ssa : Func.t -> Label.t list Temp.Table.t
  val compute_non_ssa : pred_table:Adj_table.t -> Func.t -> Live_set.t * Live_set.t

  (* below functions act on ssa form *)
  val compute : pred_table:Adj_table.t -> Func.t -> Live_set.t * Live_set.t

  val compute_next_use_distance
    :  pred_table:Adj_table.t
    -> Func.t
    -> Next_use_table.t * Next_use_table.t

  val compute_def_use_labels
    :  Func.t
    -> Label.t Temp.Table.t * (Block.t * Instr'.t) list Temp.Table.t

  val compute_deaths : live_out:Temp.Set.t -> Block.t -> Temp.Set.t iarray
end
