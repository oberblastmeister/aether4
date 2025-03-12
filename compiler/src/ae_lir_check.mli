open Std
module Lir := Ae_lir_types

val check : Lir.Func.t -> unit Or_error.t
