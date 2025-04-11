open Std
open Ae_generic_ir_import

module Make
    (Ir : Ir)
    (_ : sig
       open Make_std(Ir)

       val move : dst:Temp.t -> src:Temp.t -> ty:Ty.t -> Instr.t
     end) : sig
  open Make_std(Ir)

  (* critical edges must be split before calling this function *)
  val destruct
    :  in_same_reg:(Temp.t -> Temp.t -> bool)
    -> get_scratch:(unit -> Temp.t)
    -> Func.t
    -> Func.t
end
