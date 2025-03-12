open Ae_abs_x86_types
module Stack_builder := Ae_stack_builder
module Mach_reg := Ae_x86_mach_reg
module Entity := Ae_entity_std
module Int_table := Entity.Table.Int_table

module Allocation : sig
  type t

  val find_exn : t -> Vreg.t -> Mach_reg.t
end

val spill_instr
  :  Stack_slot.t Int_table.t
  -> (int -> Vreg.t * Stack_slot.t)
  -> int Vreg.Table.t
  -> Instr.t
  -> Instr.t list * Instr.t * Instr.t list

val alloc_func : Stack_builder.t -> Func.t -> Allocation.t * Func.t
