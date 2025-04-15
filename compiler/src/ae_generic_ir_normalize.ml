(* TODO: test this file *)
(* TODO: also normalize other stuff like removing dead code after jumps, removing unreferenced blocks, etc *)
open Std
open Ae_generic_ir_import

module Make (Ir : Ir) = struct
  open Ir

  let normalize func =
    let edit = Multi_edit.create () in
    begin
      let@: block = Func.iter_blocks func in
      let block_params = Block.find_block_params block in
      if block_params.index <> 0
      then begin
        Multi_edit.add_edits
          edit
          block.label
          [ Edit.remove block_params; Edit.insert { block_params with index = 0 } ]
      end
    end;
    Func.apply_multi_edit ~no_sort:() edit func
  ;;
end