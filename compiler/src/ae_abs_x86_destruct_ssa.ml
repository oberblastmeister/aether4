(* TODO: integrate this into the lower_flat_x86 pass *)
open Std
open Ae_abs_x86_types

let destruct ~mach_reg_gen ~allocation (func : Func.t) =
  let edit = Multi_edit.create () in
  begin
    let@: block = Func.iter_blocks func in
    let jump_instr = Block.find_control block in
    let num_block_calls = Instr.iter_block_calls jump_instr.i |> Iter.length in
    let jump_instr =
      let@: block_call = (Instr'.map & Instr.map_block_calls) jump_instr in
      let dst_block = Func.find_block_exn func (Block_call.label block_call) in
      let dst_block_params_instr = Block.find_block_params dst_block in
      Multi_edit.add_replace
        edit
        dst_block.label
        { dst_block_params_instr with i = Instr.Block_params [] };
      let dst_block_params =
        Instr.block_params_val dst_block_params_instr.i |> Option.value_exn
      in
      let parallel_moves =
        List.zip_exn dst_block_params block_call.args
        |> List.map ~f:(fun (param, src) ->
          ({ dst = param.param; src; ty = param.ty } : Sequentialize_location.Move.t))
      in
      let sequential_moves =
        Sequentialize_location.run ~allocation ~mach_reg_gen parallel_moves
      in
      (* TODO: test both of these cases *)
      if num_block_calls = 1
      then begin
        Multi_edit.add_inserts
          edit
          block.label
          (List.map sequential_moves ~f:(fun move -> Instr'.create move jump_instr.index))
      end
      else begin
        Multi_edit.add_inserts
          edit
          dst_block.label
          (List.map sequential_moves ~f:(fun move ->
             Instr'.create move (dst_block_params_instr.index + 1)))
      end;
      { label = Block_call.label block_call; args = [] }
    in
    Multi_edit.add_replace edit block.label jump_instr
  end;
  Func.apply_multi_edit ~no_sort:() edit func
;;
