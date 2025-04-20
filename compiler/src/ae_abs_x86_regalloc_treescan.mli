open Std
open Ae_abs_x86_types

val alloc_func : colors:Int.Set.t -> Func.t -> int Temp.Table.t
