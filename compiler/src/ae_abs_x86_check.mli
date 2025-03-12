open Std
module Abs_x86 := Ae_abs_x86_types

val check : Abs_x86.Func.t -> unit Or_error.t
