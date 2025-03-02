open Std
open Ae_generic_ir_import

module Make
    (Ir : Ir)
    (_ : sig
       open Ir.Std

       val move : dst:Temp.t -> src:Temp.t -> ty:Ty.t -> Instr.t
     end) : sig
  open Ir.Std

  module Move : sig
    type t =
      { dst : Temp.t
      ; src : Temp.t
      ; ty : Ty.t
      }
    [@@deriving sexp_of]
  end

  val sequentialize_parallel_moves
    :  in_same_reg:(Temp.t -> Temp.t -> bool)
    -> get_scratch:(unit -> Temp.t)
    -> Move.t list
    -> Move.t list

  val destruct : in_same_reg:(Temp.t -> Temp.t -> bool) -> Func.t -> Func.t
end
