open Std
open Ae_abs_x86_types
module Stack_slot := Ae_stack_slot

type t [@@deriving sexp_of]

(* resolve offset from the base pointer *)
val resolve_frame_base_offset : t -> Stack_slot.t -> int

(* the total size of the frame, how much to increase rsp by *)
val frame_size : t -> int
val compute : Func.t -> t
