open Std
open Ae_generic_ir_import

module Make (Ir : Ir) : sig
  open Make_std(Ir)

  val convert : Func.t -> Func.t
end
