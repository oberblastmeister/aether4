open Std
open Ae_abs_x86_types
module Stack_slot_entity = Ae_stack_slot_entity
module Entity = Ae_entity_std
module Ident = Entity.Ident
module Table = Ident.Table
module Stack_slot = Stack_slot_entity.Ident
open Ae_trace

type t =
  { (* basically a prefix sum of offsets *)
    table : int Stack_slot.Table.t
  ; frame_size : int
  }
[@@deriving sexp_of]

let resolve_frame_base_offset t slot =
  let open Table.Syntax in
  t.table.!(slot)
;;

let frame_size t = t.frame_size

let compute (func : Func.t) =
  let open Table.Syntax in
  let offset = ref 0 in
  let layout_table = Table.create () in
  begin
    let@: slot, size = List.iter (List.rev func.data.stack_slots) in
    offset := !offset - Int.round_up ~to_multiple_of:8 (Ty.to_bytes size);
    layout_table.!(slot) <- !offset
  end;
  (* since the stack is 16 byte aligned before the call,
     it is 16 byte aligned inside the call,
     due to the return address and base pointer being pushed
  *)
  let align size =
    assert (size % 8 = 0);
    if size % 16 = 8 then size + 8 else size
  in
  let frame_size = align (Int.neg !offset) in
  { table = layout_table; frame_size }
;;
