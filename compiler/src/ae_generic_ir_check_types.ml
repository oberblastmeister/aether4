open Std
module Entity = Ae_entity_std
module Label = Ae_label_entity.Ident
open Ae_generic_ir_sigs

(* TODO: check calls when we add those *)
module Make (Ir : Ir) = struct
  open Make_std (Ir)

  exception Exn of Error.t

  let throw_s s = raise (Exn (Error.create_s s))

  let check_temp_ty ty_table temp ty =
    let open Entity.Ident.Table.Syntax in
    if not (Ty.equal ty_table.!(temp) ty)
    then throw_s [%message "Type mismatch" (temp : Temp.t) (ty : Ty.t)]
  ;;

  let check_block_call ty_table func (block_call : Block_call.t) =
    let dst_block = Func.find_block_exn func (Block_call.label block_call) in
    let block_params =
      Block.find_block_params dst_block
      |> Instr'.instr
      |> Instr.block_params_val
      |> Option.value_exn in
    let block_param_tys = List.map block_params ~f:Block_param.ty
    in
    let temp_with_ty =
      (* TODO: fix this and use the Location *)
      let args = Iter.to_list (Block_call.iter_uses block_call) in
      match List.zip args block_param_tys with
      | Ok t -> t
      | Unequal_lengths ->
        throw_s
          [%message
            "Didn't provide the right number of block args"
              (dst_block.label : Label.t)
              (args : Location.t list)
              (block_param_tys : Ty.t list)]
    in
    begin
      let@: location, ty = List.iter temp_with_ty in
      let@: temp = Option.iter @@ Location.temp_val location in
      check_temp_ty ty_table temp ty
    end;
    ()
  ;;

  let check_instr ty_table func (instr : Instr.t) =
    begin
      let@: temp, ty = Instr.iter_uses_with_known_ty instr in
      check_temp_ty ty_table temp ty
    end;
    begin
      let@: block_call = Instr.iter_block_calls instr in
      check_block_call ty_table func block_call
    end
  ;;

  let check' (func : Func.t) =
    let ty_table = Func.get_ty_table func in
    let@: block = Func.iter_blocks func in
    let@: instr = Block.iter_fwd block in
    check_instr ty_table func instr.i
  ;;

  let check func =
    let open Or_error.Let_syntax in
    match check' func with
    | exception Exn e -> Error e
    | () -> Ok ()
  ;;
end
