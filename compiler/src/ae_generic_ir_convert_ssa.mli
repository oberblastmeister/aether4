open Std
open Ae_generic_ir_import

module Make (Ir : Ir) : sig
  open Ir

  val convert : ?renumber:unit -> Func.t -> Func.t
end
