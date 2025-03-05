open Std
module Stack_slot_entity = Ae_stack_slot_entity
module Entity = Ae_entity_std
module Ident = Entity.Ident
module Stack_slot = Stack_slot_entity.Ident
module Size = Ae_x86_size

type t = { table : Size.t Stack_slot.Table.t }

let create () =
  let table = Ident.Table.create () in
  { table }
;;

let alloc ?name t size =
  let slot = Ident.add_table ?name size t.table in
  slot
;;

let to_list t = Ident.Table.to_list t.table
