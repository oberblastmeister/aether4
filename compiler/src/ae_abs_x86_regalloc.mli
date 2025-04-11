open Ae_abs_x86_types
module Frame := Ae_x86_frame
module Mach_reg := Ae_x86_mach_reg
module Entity := Ae_entity_std
module Int_table := Entity.Table.Int_table

module Allocation : sig
  type t

  val find_exn : t -> Temp.t -> Mach_reg.t
end

(* val spill_instr
  :  spilled_color_to_slot:Stack_slot.t Int_table.t
  -> get_evicted_temp_and_slot_for_mach_reg:(Mach_reg.t -> Temp.t * Stack_slot.t)
  -> coloring:int Temp.Table.t
  -> instr:Instr.t
  -> Instr.t list * Instr.t * Instr.t list *)

val alloc_func : Frame.Builder.t -> Func.t -> Allocation.t * Func.t
