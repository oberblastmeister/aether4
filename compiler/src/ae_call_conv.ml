open Std

open struct
  module Mach_reg = Ae_x86_mach_reg
end

type t =
  { name : string
  ; return_regs : Mach_reg.t list
  ; call_args : Mach_reg.t list
  ; num_args_in_regs : int
  ; call_clobbers : Mach_reg.t list
  }
[@@deriving sexp_of]
