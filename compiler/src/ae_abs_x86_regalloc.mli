open Ae_abs_x86_types
module Stack_builder := Ae_stack_builder
module Mach_reg := Ae_x86_mach_reg

module Allocation : sig
  type t = Alloc_reg.t Vreg.Table.t [@@deriving sexp_of]
end

val alloc_func : Stack_builder.t -> Func.t -> Allocation.t
