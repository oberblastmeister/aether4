open Std
open Ae_abs_x86_types

val spill_func
  :  mach_reg_id:Temp_entity.Id.t
  -> get_temp_color:(Temp.t -> int)
  -> spilled_colors:Int.Set.t
  -> Func.t
  -> Func.t
