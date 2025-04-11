open Std
open Ae_generic_ir_sigs
module Entity = Ae_entity_std
module Ident = Entity.Ident
module Label = Ae_label_entity.Ident
module Stack_slot = Ae_stack_slot_entity.Ident
module Table = Ident.Table
open Table.Syntax

module Make
    (Ir : Ir)
    (Instr_ext : sig
       open Make_std(Ir)

       module Block_call : sig
         open Block_call

         module Arg : sig
           type t =
             | Temp of Temp.t
             | Slot of Stack_slot.t
         end

         val get_args : t -> Arg.t list
         val map_args : t -> f:(Arg.t list -> Arg.t list) -> t
         val with_args : t -> Arg.t list -> t
         val create : Label.t -> Arg.t list
       end

       val spill_temp : Temp.t -> Instr.t
       val reload_temp : Temp.t -> Instr.t
     end) =
struct
  open Make_std(Ir)
  module Liveness = Ae_generic_ir_liveness.Make (Ir)

  let init_usual ~num_regs ~preds ~reg_out_table =
    let preds_num = List.length preds in
    let temp_freq = Table.create () in
    let in_all = ref Ident.Set.empty in
    let in_some = ref Ident.Set.empty in
    begin
      let@: pred = List.iter preds in
      begin
        let@: temp = Ident.Set.iter reg_out_table.!(pred) in
        temp_freq.!(temp) <- temp_freq.!(temp) + 1;
        in_some := Ident.Set.add !in_some temp;
        if temp_freq.!(temp) = preds_num
        then begin
          in_some := Ident.Set.remove !in_some temp;
          in_all := Ident.Set.add !in_all temp
        end
      end
    end;
    let in_all = !in_all in
    let in_some = !in_some in
    let temps_to_add =
      Ident.Set.to_list in_some
      |> List.take __ (Ident.Set.length in_all - num_regs)
      |> Ident.Set.of_list_exn
    in
    Ident.Set.union in_all temps_to_add
  ;;

  let spill_block
        ~num_regs
        ~reg_in_table
        ~reg_out_table
        ~pred_table
        ~next_use_out_table
        ~reg_num
        ~multi_edit
        (block : Block.t)
    =
    let next_uses_at_point =
      let next_use = ref next_use_out_table.!(block.label) in
      Arrayp.mapi_rev block.body ~f:(fun _ instr ->
        next_use := Liveness.next_use_backwards_transfer instr.i !next_use;
        !next_use)
    in
    let reg_in = init_usual ~num_regs ~preds:pred_table.!(block.label) ~reg_out_table in
    (* let all_in_registers = () in
    let temps_in_registers = ref Ident.Set.empty in *)
    begin
      let@: instr = Block.iter_fwd block in
      let uses = Instr.iter_uses instr.i |> Iter.to_list in
      (* TODO:
          algorithm for uses in block calls:
          don't worry if they are spilled, just ignore
      *)
      ()
    end;
    todo ()
  ;;

  let spill ~reg_num (func : Func.t) =
    let pred_table = Func.pred_table func in
    let def_to_label, use_to_labels = Liveness.compute_def_use_labels func in
    let next_use_in, next_use_out = Liveness.compute_next_use_distance ~pred_table func in
    let labels_order = Func.labels_reverse_postorder func in
    let spilled = Hashtbl.create (module Temp) in
    begin
      let@: label = Vec.iter labels_order in
      let block = Func.find_block_exn func label in
      ()
    end;
    todo ()
  ;;
end
