module Stack_slot_entity := Ae_stack_slot_entity
module Stack_slot := Stack_slot_entity.Ident

type t

val create : unit -> t
val alloc : ?name:string -> t -> int -> Stack_slot.t
val to_list : t -> (Stack_slot.t * int) list
