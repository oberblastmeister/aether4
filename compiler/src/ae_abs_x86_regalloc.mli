open Ae_abs_x86_types
module Stack_builder := Ae_stack_builder
module Mach_reg := Ae_x86_mach_reg

module Allocation : sig
  type t

  val find_exn : t -> Vreg.t -> Mach_reg.t
end

val alloc_func : Stack_builder.t -> Func.t -> Allocation.t * Func.t
