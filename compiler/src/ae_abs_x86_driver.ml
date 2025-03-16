module C0 = Ae_c0_std
module Lower_flat_x86 = Ae_abs_x86_lower_flat_x86
module Regalloc = Ae_abs_x86_regalloc
module Flat_x86 = Ae_flat_x86_std
module Frame = Ae_x86_frame
open Ae_abs_x86_types

let convert func =
  let frame_builder = Frame.Builder.create () in
  let alloc, func = Regalloc.alloc_func frame_builder func in
  let func = Lower_flat_x86.lower (Frame.Builder.to_layout frame_builder) alloc func in
  func
;;
