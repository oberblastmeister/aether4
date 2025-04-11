open Std
open Ae_generic_ir_import

module Make (Ir : Ir) = struct
  open Make_std(Ir)

  exception Exn of Error.t

  let throw_s s = raise (Exn (Error.create_s s))

  let check_block (block : Block.t) =
    let block_params =
      Arrayp.filter block.body ~f:(fun instr -> Instr.is_block_params instr.i)
    in
    if Arrayp.length block_params <> 1
    then
      throw_s [%message "Expected a single block param" (block_params : Instr'.t iarray)];
    let jump = Arrayp.find block.body ~f:(fun instr -> Instr.is_control instr.i) in
    if Option.is_none jump
    then throw_s [%message "Expected at least one jump instruction"];
    ()
  ;;

  let check' func =
    let@: block = Func.iter_blocks func in
    check_block block
  ;;

  let check func =
    match check' func with
    | exception Exn e -> Error e
    | _ -> Ok ()
  ;;
end
