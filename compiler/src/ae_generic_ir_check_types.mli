open Std
open Ae_generic_ir_sigs

module Make (Ir : Ir) : sig
  open Make_std(Ir)

  val check : Func.t -> unit Or_error.t
end
