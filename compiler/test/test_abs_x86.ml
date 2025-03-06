open Std
open Aether4
open Ae_abs_x86_std
module Dominators = Ae_dominators
module Entity = Ae_entity_std
module Ident = Entity.Ident
module Vreg_intern = Entity.Intern.String_to_name.Make_global (Vreg_entity.Witness) ()
module Label_intern = Entity.Intern.String_to_name.Make_global (Label_entity.Witness) ()

let vreg = Vreg_intern.intern
let lab = Label_intern.intern

let%expect_test _ =
  let instr = Instr.Jump { Block_call.label = lab "bruh"; args = [ vreg "first" ] } in
  let uses = Instr.iter_uses instr |> Iter.to_list in
  print_s [%message (uses : Vreg.t list)];
  [%expect {| (uses (first@0)) |}]
;;
