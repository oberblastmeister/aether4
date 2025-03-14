module Stack_slot_entity := Ae_stack_slot_entity
module Stack_slot := Stack_slot_entity.Ident
module Size := Ae_x86_size

module Layout : sig
  type t [@@deriving sexp_of]

  (* resolve offset from the base pointer *)
  val resolve_frame_offset : t -> Stack_slot.t -> int

  (* the total size of the frame, how much to increase rsp by *)
  val frame_size : t -> int
end

module Builder : sig
  type t [@@deriving sexp_of]

  val create : unit -> t
  val alloc : ?name:string -> t -> Size.t -> Stack_slot.t
  val to_layout : t -> Layout.t
  val to_list : t -> (Stack_slot.t * Size.t) list
end
