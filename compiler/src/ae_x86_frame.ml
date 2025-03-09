open Std
module Stack_slot_entity = Ae_stack_slot_entity
module Entity = Ae_entity_std
module Ident = Entity.Ident
module Stack_slot = Stack_slot_entity.Ident
module Size = Ae_x86_size
module Table = Ident.Table

module Layout = struct
  type t =
    { (* basically a prefix sum of offsets *)
      table : int Stack_slot.Table.t
    ; frame_size : int
    }
  [@@deriving sexp_of]

  let resolve_base_offset t slot = todo ()
  let frame_size _ = todo ()
end

module Builder = struct
  type t = { table : Size.t Stack_slot.Table.t } [@@deriving sexp_of]

  let create () =
    let table = Table.create () in
    { table }
  ;;

  let alloc ?name t size =
    let slot = Ident.add_table ?name size t.table in
    slot
  ;;

  let to_list t = Table.to_list t.table

  (* todo: get rid of the base pointers *)
  let to_layout t =
    let open Table.Syntax in
    let offset = ref 0 in
    let layout_table = Table.create () in
    begin
      let@: slot, size = Table.iteri t.table in
      layout_table.!(slot) <- !offset;
      offset := !offset + Int.round_up ~to_multiple_of:8 (Size.to_bytes size)
    end;
    (* (* make sure to align to 16 bits so calls are correct *)
    (* since the stack is 16 byte aligned before the call,
       it is only 8 byte aligned in the call,
       due to the return address being pushed *)
    let align size =
      assert (size % 8 = 0);
      if size % 16 = 0 then size + 8 else size
    in *)
    let frame_size = todo () in
    { Layout.table = layout_table; frame_size }
  ;;
end
