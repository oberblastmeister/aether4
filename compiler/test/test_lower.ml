open Std
open Aether4
module Driver = Ae_driver
module C0 = Ae_c0_std
module Tir = Ae_tir_std
module Lir = Ae_lir_lower_abs_x86
module Abs_x86 = Ae_abs_x86_std
module Flat_x86 = Ae_flat_x86_std

let check s =
  let tir = Driver.compile_source_to_tir s |> Or_error.ok_exn in
  let lir = Tir.Lower_lir.lower tir in
  let abs_x86 = Lir.lower lir in
  let asm = Abs_x86.Driver.convert abs_x86 in
  let formatted_asm = Flat_x86.Format.format asm in
  print_endline formatted_asm;
  ()
;;
