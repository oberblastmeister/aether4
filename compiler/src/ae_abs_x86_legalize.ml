(*
   we must do this before register allocation,
  so that register constraints work properly
*)
open Std
open Ae_abs_x86_types
module Entity = Ae_entity_std
module Id_gen = Entity.Id_gen
module Ident = Entity.Ident

let legalize_block ~edit ~temp_gen block =
  begin
    let@: instr' = Block.iter_fwd block in
    match instr'.i with
    | Instr.Bin { dst; op = (Idiv | Imod) as op; src1; src2 } ->
      (match src1 with
       | Reg _ -> ()
       | _ ->
         let temp = Ident.fresh ~name:"legalize" temp_gen in
         Multi_edit.add_insert
           edit
           block.label
           (Instr'.create
              (Instr.Mov { dst = Reg temp; src = src1; size = Qword })
              instr'.index);
         Multi_edit.add_replace
           edit
           block.label
           { instr' with i = Instr.Bin { dst; op; src1 = Reg temp; src2 } };
         ());
      (match dst with
       | Reg _ -> ()
       | _ ->
         let temp = Ident.fresh ~name:"legalize" temp_gen in
         Multi_edit.add_replace
           edit
           block.label
           { instr' with i = Instr.Bin { dst = Reg temp; op; src1; src2 } };
         Multi_edit.add_insert
           edit
           block.label
           (Instr'.create
              (Instr.Mov { dst; src = Reg temp; size = Qword })
              (instr'.index + 1));
         ());
      ()
    | _ -> ()
  end;
  ()
;;

let legalize_func func =
  let edit = Multi_edit.create () in
  let temp_gen = Func.create_temp_gen func in
  begin
    let@: block = Func.iter_blocks func in
    legalize_block ~edit ~temp_gen block;
    ()
  end;
  Func.apply_multi_edit edit func |> Func.apply_temp_gen temp_gen
;;
