open Std
open Ae_generic_ir_import
module Mach_reg = Ae_x86_mach_reg

module Make
    (Ir : Ir)
    (Instr_ext : sig
       open Make_std(Ir)

       val move : dst:Temp.t -> src:Temp.t -> ty:Ty.t -> Instr.t
     end) =
struct
  open Make_std(Ir)

  module Sequentialize_parallel_moves = Ae_sequentialize_parallel_moves.Make (struct
      module Temp = Temp
      module Ty = Ty
    end)

  open Sequentialize_parallel_moves

  let move_to_instr { Move.dst; src; ty } = Instr_ext.move ~dst ~src ~ty
  let destruct ~in_same_reg ~get_scratch (func : Func.t) = todol [%here]
  (* let edit = Multi_edit.create () in
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
          { dst_block_params_instr with i = Instr.block_params ~temps:[] };
        let (`temps dst_block_params) =
          Instr.block_params_val dst_block_params_instr.i |> Option.value_exn
        in
        let parallel_moves =
          let args = Block_call.iter_uses block_call |> Iter.to_list in
          List.zip_exn dst_block_params args
          |> List.map ~f:(fun ((dst, ty), src) -> { Move.dst; src; ty })
        in
        let sequential_moves =
          Sequentialize_parallel_moves.sequentialize
            ~in_same_reg
            ~get_scratch
            parallel_moves
        in
        (* TODO: test both of these cases *)
        if num_block_calls = 1
        then begin
          Multi_edit.add_inserts
            edit
            block.label
            (List.map sequential_moves ~f:(fun move ->
               Instr'.create (move_to_instr move) jump_instr.index))
        end
        else begin
          Multi_edit.add_inserts
            edit
            dst_block.label
            (List.map sequential_moves ~f:(fun move ->
               Instr'.create (move_to_instr move) (dst_block_params_instr.index + 1)))
        end;
        Block_call.create (Block_call.label block_call) []
      in
      Multi_edit.add_replace edit block.label jump_instr
    end;
    { func with blocks = Multi_edit.apply_blocks ~no_sort:() edit func.blocks } *)
end
