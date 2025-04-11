open Std
open Ae_abs_x86_types

module Sequentialize_parallel_moves = Ae_sequentialize_parallel_moves.Make (struct
    module Temp = Temp
    module Ty = Ty
  end)

open Sequentialize_parallel_moves

let move_to_instr { Move.dst; src; ty } = Instr.move ~dst ~src ~ty

let destruct ~in_same_reg ~get_scratch (func : Func.t) =
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
      let (`params dst_block_params) =
        let params =
          Instr.block_params_val dst_block_params_instr.i |> Option.value_exn
        in
        `params params
      in
      let module M = struct
        type ('d, 's) t =
          { dst : 'd
          ; src : 's
          ; size : Ty.t
          }
      end
      in
      let parallel_moves =
        List.zip_exn dst_block_params block_call.args
        |> List.map ~f:(fun (param, src) ->
          ({ dst = param.param; src; size = param.ty } : _ M.t))
      in
      let moves_slot_loc, moves_temp_temp, moves_loc_slot =
        List.partition3_map parallel_moves ~f:(fun move ->
          match move.dst, move.src with
          | Slot _, _ -> `Fst move
          | Temp dst, Temp src -> `Snd { move with dst; src }
          | _, Slot _ -> `Trd move)
      in
      let sequential_moves =
        Sequentialize_parallel_moves.sequentialize
          ~in_same_reg
          ~get_scratch
          (List.map moves_temp_temp ~f:(fun { dst; src; size } ->
             { Move.dst; src; ty = size }))
      in
      let sequential_moves =
        let into (t : _ M.t) =
          Instr.Mov
            { dst = Location.to_operand t.dst
            ; src = Location.to_operand t.src
            ; size = t.size
            }
        in
        (* these go before so the loc which may be temp does not get clobbered *)
        List.map moves_slot_loc ~f:into
        @ List.map sequential_moves ~f:move_to_instr
        (* These go after so we don't clobber the loc which may be temp *)
        @ List.map moves_loc_slot ~f:into
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
  { func with blocks = Multi_edit.apply_blocks ~no_sort:() edit func.blocks }
;;
