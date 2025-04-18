open Std
open Ae_abs_x86_types

val spill_func
  :  mach_reg_gen:Mach_reg_gen.t
  -> get_temp_color:(Temp.t -> int)
  -> spilled_colors:Int.Set.t
  -> Func.t
  -> Func.t
