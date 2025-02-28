open Std
open Ae_generic_ir_sigs
module Label_entity := Ae_label_entity
module Label := Label_entity.Ident

module Make (Ir : Ir) : sig
  open Ir
  module Temp_entity := Ir.Arg
  module Temp := Ir.Arg.Temp_entity.Ident

  module Live_list : sig
    type t = Temp.t list Label.Table.t [@@deriving sexp_of]
  end

  module Live_set : sig
    type t = Temp.Set.t Label.Table.t [@@deriving sexp_of]
  end

  val compute_non_ssa_live_list : Adj_table.t -> Func.t -> Live_list.t * Live_list.t
  val compute_non_ssa : Adj_table.t -> Func.t -> Live_set.t * Live_set.t
end
