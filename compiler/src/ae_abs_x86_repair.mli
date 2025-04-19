open Std
open Ae_abs_x86_types

val repair_func
  :  allocation:int Temp.Table.t
  -> mach_reg_gen:Mach_reg_gen.t
  -> Func.t
  -> Func.t
