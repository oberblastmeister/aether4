module Stack_slot_entity := Ae_stack_slot_entity
module Stack_slot := Stack_slot_entity.Ident
module Size := Ae_x86_size

type t

val create : unit -> t
val alloc : ?name:string -> t -> Size.t -> Stack_slot.t
val to_list : t -> (Stack_slot.t * Size.t) list
