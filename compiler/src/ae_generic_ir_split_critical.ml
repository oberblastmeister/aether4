open Std
open Ae_generic_ir_import

module Make (Ir : Ir) = struct
  open Ir.Std

  let split (func : Func.t) =
    let module Table = Ident.Table in
    let open Ident.Table.Syntax in
    let pred_table = Func.pred_table func in
    let edit = Multi_edit.create () in
    let label_gen = Id_gen.of_id func.next_label_id in
    begin
      let@: block = Func.iter_blocks func in
      let jump_instr = Block.find_jump block in
      let block_calls = Instr.iter_block_calls jump_instr.i |> Iter.to_list in
      if List.length block_calls > 1
      then begin
        let jump_instr_ref = ref jump_instr in
        begin
          let@: block_call = List.iter block_calls in
          let preds = pred_table.!(block_call.label) in
          if List.length preds > 1
          then begin
            let new_label = Ident.freshen label_gen block_call.label in
            Multi_edit.add_insert
              edit
              new_label
              (Instr'.create (Instr.block_params ~temps:[]) 0);
            (* this block jumps to the original destination *)
            Multi_edit.add_insert
              edit
              new_label
              (Instr'.create
                 (Instr.jump (Block_call.create block_call.label ~args:block_call.args))
                 0);
            (* override the original jump to the new label *)
            jump_instr_ref
            := begin
                 let@: instr = Instr'.map !jump_instr_ref in
                 Instr.map_block_calls instr ~f:(fun block_call' ->
                   if Label.equal block_call'.label block_call.label
                   then { block_call' with label = new_label; args = [] }
                   else block_call')
               end
          end;
          if not (phys_equal jump_instr !jump_instr_ref)
          then Multi_edit.add_replace edit block.label !jump_instr_ref
        end
      end
    end;
    { func with
      blocks = Multi_edit.apply_blocks ~no_sort:() edit func.blocks
    ; next_label_id = Id_gen.next label_gen
    }
  ;;
end
