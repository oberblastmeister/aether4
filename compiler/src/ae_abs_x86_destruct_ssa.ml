(* TODO: integrate this into the lower_flat_x86 pass *)
open Std
module Entity = Ae_entity_std
module Ident = Entity.Ident
open Ae_abs_x86_types

module Sequentialize_parallel_moves = Ae_sequentialize_parallel_moves.Make (struct
    module Temp = Location
    module Ty = Ty
  end)

open Sequentialize_parallel_moves

let mach_reg_id off mach_reg = Entity.Id.offset off (Mach_reg.to_enum mach_reg)

let mach_reg_ident ?info off mach_reg =
  let id = mach_reg_id off mach_reg in
  Ident.create ?info (Mach_reg.to_string mach_reg) id
;;

let destruct ~mach_reg_gen ~in_same_reg ~get_scratch (func : Func.t) =
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
      let sequential_moves, did_use_scratch =
        Sequentialize_parallel_moves.sequentialize
          ~in_same_reg
          ~get_scratch
          (List.map parallel_moves ~f:(fun { dst; src; size } ->
             { Move.dst; src; ty = size }))
      in
      let sequential_moves =
        List.concat_map sequential_moves ~f:(fun move ->
          match move.dst, move.src with
          | Slot slot1, Slot slot2 when did_use_scratch ->
            let r10 = Mach_reg_gen.get mach_reg_gen R10 in
            [ Instr.Push { src = r10; size = Qword }
            ; Instr.Mov { dst = Reg r10; src = Stack_slot slot2; size = move.ty }
            ; Instr.Mov { dst = Stack_slot slot1; src = Reg r10; size = move.ty }
            ; Instr.Pop { dst = r10; size = Qword }
            ]
          | Slot slot1, Slot slot2 ->
            let r11 = Mach_reg_gen.get mach_reg_gen R11 in
            [ Instr.Mov { dst = Reg r11; src = Stack_slot slot2; size = move.ty }
            ; Instr.Mov { dst = Stack_slot slot1; src = Reg r11; size = move.ty }
            ]
          | _ ->
            [ Instr.Mov
                { dst = Location.to_operand move.dst
                ; src = Location.to_operand move.src
                ; size = move.ty
                }
            ])
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
