open Std

open struct
  module Sigs = Ae_generic_ir_sigs
  module Temp = Ae_temp
end

module Make (Ir : Sigs.Ir_simple) = struct
  include Ir

  module Instr_ext = struct
    open Instr

    let iter_labels i = Instr.iter_block_calls i |> Iter.map ~f:Block_call.label
    let labels_list i = iter_labels i |> Iter.to_list
  end

  module Func_ext = struct
    open Func

    let get_ty_table func =
      let table = Temp.Table.create () in
      begin
        let@: block = iter_blocks func in
        let@: instr = Block.iter_fwd block in
        let@: def, ty = Instr.iter_defs_with_ty instr.i in
        if not (Temp.Table.mem table def)
        then begin
          table.Temp.!(def) <- ty
        end
        else begin
          assert (Ty.equal table.Temp.!(def) ty)
        end
      end;
      table
    ;;
  end
end
