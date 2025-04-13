open Std
open Ae_abs_x86_types
module Stack_slot_entity := Ae_stack_slot_entity
module Stack_slot := Stack_slot_entity.Ident

type t [@@deriving sexp_of]

(* resolve offset from the base pointer *)
val resolve_frame_offset : t -> Stack_slot.t -> int

(* the total size of the frame, how much to increase rsp by *)
val frame_size : t -> int
val compute : Func.t -> t
