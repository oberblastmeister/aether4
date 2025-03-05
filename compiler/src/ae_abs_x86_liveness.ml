(* open Std
open Ae_abs_x86_types
module Use_defs = Ae_abs_x86_use_defs
module Entity = Ae_entity_std
module Table = Entity.Ident.Table
open Table.Syntax

type t = unit Vreg.Table.t

let transfer liveness instr =
  let uses = Use_defs.Instr.iter_uses instr |> Iter.to_list in
  let defs = Use_defs.Instr.iter_defs instr |> Iter.to_list in
  List.iter defs ~f:(fun def ->
    Table.remove liveness def;
    ());
  List.iter uses ~f:(fun use ->
    liveness.!(use) <- ();
    ());
  ()
;; *)
