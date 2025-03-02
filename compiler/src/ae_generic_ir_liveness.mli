open Std
open Ae_generic_ir_import

module Make (Ir : Ir) : sig
  open Ir.Std

  module Live_list : sig
    type t = Temp.t list Label.Table.t [@@deriving sexp_of]
  end

  module Live_set : sig
    type t = Temp.Set.t Label.Table.t [@@deriving sexp_of]
  end

  val compute_def_blocks : Func.t -> Label.t list Temp.Table.t * Ty.t Temp.Table.t

  val compute_non_ssa_live_list
    :  pred_table:Adj_table.t
    -> Func.t
    -> Live_list.t * Live_list.t

  val compute_non_ssa : pred_table:Adj_table.t -> Func.t -> Live_set.t * Live_set.t
end
