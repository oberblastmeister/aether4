open Std
open Ae_abs_x86_types
module Mach_reg := Ae_x86_mach_reg
module Entity := Ae_entity_std
module Int_table := Entity.Table.Int_table

module Allocation : sig
  type t

  val find_exn : t -> Temp.t -> Mach_reg.t
  val find_color_exn : t -> Temp.t -> int
end

val alloc_func
  :  mach_reg_id:Temp_entity.Id.t
  -> Func.t
  -> Allocation.t * Int.Set.t * Func.t
