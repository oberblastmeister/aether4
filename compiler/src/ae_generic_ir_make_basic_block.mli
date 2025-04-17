open Std
module Intf := Ae_generic_ir_make_intf
module Make (Arg : Intf.Arg) : Intf.S with module Arg := Arg
