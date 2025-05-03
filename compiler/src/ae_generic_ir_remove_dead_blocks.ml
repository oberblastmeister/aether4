open Std
open Ae_generic_ir_import

module Make (Ir : Ir) : sig
  open Ir

  val run : Func.t -> Func.t
end = struct
  open Ir

  let remove_after_control_flow block =
    let control = Block.find_control block in
    if control.index <> Arrayp.length block.body - 1
    then begin
      let new_body =
        Arrayp.to_list block.body |> List.take __ (control.index + 1) |> Arrayp.of_list
      in
      Block.set_instrs new_body block
    end
    else block
  ;;

  let run (func : Func.t) =
    let reachable_labels =
      Func.labels_postorder func |> Vec.to_list |> Label.Hash_set.of_list
    in
    let new_blocks =
      Func.blocks func
      |> Map.filter_keys ~f:(Hash_set.mem reachable_labels)
      |> Map.map ~f:remove_after_control_flow
    in
    Func.set_blocks func new_blocks
  ;;
end
