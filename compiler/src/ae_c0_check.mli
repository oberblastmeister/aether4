open Std
module Cst := Ae_c0_cst

val check_program : Cst.program -> unit Or_error.t
