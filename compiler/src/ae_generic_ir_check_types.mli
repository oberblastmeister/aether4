open Std
open Ae_generic_ir_sigs

module Make (Ir : Ir) : sig
  open Ir

  val check : Func.t -> unit Or_error.t
end
