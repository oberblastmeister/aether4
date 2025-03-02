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

  val backwards_transfer : Instr.t -> Temp.Set.t -> Temp.Set.t
  val compute_def_blocks_non_ssa : Func.t -> Label.t list Temp.Table.t
  val compute_ty_table : Func.t -> Ty.t Temp.Table.t
  val compute_non_ssa : pred_table:Adj_table.t -> Func.t -> Live_set.t * Live_set.t

  (* below functions act on ssa form *)
  val compute : pred_table:Adj_table.t -> Func.t -> Live_set.t * Live_set.t
  val compute_def_use_labels : Func.t -> Label.t Temp.Table.t * Label.t list Temp.Table.t
end
