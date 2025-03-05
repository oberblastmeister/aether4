open Std
open Aether4
module Driver = Ae_driver
module Stack_builder = Ae_stack_builder
module C0 = Ae_c0_std
module Tir = Ae_tir_std
module Lir = Ae_lir_lower_abs_x86
module Abs_x86 = Ae_abs_x86_std
module Flat_x86 = Ae_flat_x86_std

let check s =
  let tir = Driver.compile_source_to_tir s |> Or_error.ok_exn in
  let lir = Tir.Lower_lir.lower tir in
  let abs_x86 = Lir.lower lir in
  let stack_builder = Stack_builder.create () in
  let alloc, abs_x86 = Abs_x86.Regalloc.alloc_func stack_builder abs_x86 in
  let asm = Abs_x86.Lower_flat_x86.lower alloc abs_x86 in
  let formatted_asm = Flat_x86.Format.format asm in
  print_endline formatted_asm;
  ()
;;
