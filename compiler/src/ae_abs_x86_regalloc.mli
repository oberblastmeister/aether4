open Std
open Ae_abs_x86_types
module Mach_reg := Ae_x86_mach_reg

val alloc_func : Func.t -> int Temp.Table.t * Int.Set.t
