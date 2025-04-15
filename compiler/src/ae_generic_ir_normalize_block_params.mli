open Std
open Ae_generic_ir_import

module Make (Ir : Ir) : sig
  open Ir

  val normalize : Func.t -> Func.t
end
