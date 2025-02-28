open Std
module Tir = Ae_tir_types
module Entity = Ae_entity_std
module Ident = Entity.Ident

let convert func =
  let succ_map = Tir.Func.succ_map func in
  let block_to_phis = Ident.Table.create () in
  (* (Ident.Map.iter) *)
  todo ()
;;
