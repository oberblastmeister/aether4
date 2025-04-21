open Std
open Ae_abs_x86_types
module Entity = Ae_entity_std
module Id = Entity.Id
module Ident = Entity.Ident
module Int_table = Entity.Table.Int_table
module Bitvec = Ae_data_bitvec
module Regalloc = Ae_graph_greedy_regalloc
module Graph = Regalloc.Graph
module Mach_reg = Ae_x86_mach_reg
module Id_gen = Entity.Id_gen
module Call_conv = Ae_x86_call_conv
module Chaos_mode = Ae_chaos_mode
module Table = Entity.Ident.Table
module Dominators = Ae_dominators
open Table.Syntax
open Ae_trace

let alloc_block
      ~is_start
      ~use_locations
      ~(usable_colors : Int.Set.t)
      ~live_in
      ~live_out
      ~allocation
      block
  =
  let available_colors =
    let used_colors =
      Ident.Set.iter live_in
      |> Iter.map ~f:(fun temp ->
        Table.find allocation temp
        |> Option.value_exn
             ~message:
               "Variable should have been allocated a register because we traversed the \
                blocks in dominator order")
      |> Iter.to_list
    in
    List.fold ~init:usable_colors ~f:Set.remove used_colors |> ref
  in
  let deaths = Liveness.compute_deaths ~live_out block in
  begin
    let@: instr' = Block.iter_fwd block in
    if is_start && instr'.index = 0
    then begin
      let params = Instr.block_params_val instr'.i |> Option.value_exn in
      let params_with_mach_reg, rem =
        List.zip_with_remainder params Call_conv.call_arguments
      in
      begin
        let params_slots =
          List.drop_while params ~f:(fun param -> Location.is_temp param.param)
        in
        (* make sure the suffix is all slots *)
        begin
          let@: param_slot = List.iter params_slots in
          assert (Location.is_slot param_slot.param)
        end
      end;
      begin
        match rem with
        | Some (First _) ->
          raise_s
            [%message
              "Should have spilled the excess function arguments onto the caller stack \
               frame"]
        | _ -> ()
      end;
      begin
        let@: param, mach_reg = List.iter params_with_mach_reg in
        begin
          match param.param with
          | Temp temp -> allocation.!(temp) <- Mach_reg.to_enum mach_reg
          | _ -> todol [%here]
        end
      end
    end
    else begin
      (* release dead uses *)
      begin
        let@: use =
          Instr.iter_uses instr'.i
          |> Iter.filter ~f:(fun temp -> Ident.Set.mem deaths.@(instr'.index) temp)
        in
        if Set.mem usable_colors allocation.!(use)
        then available_colors := Set.add !available_colors allocation.!(use)
      end;
      (* allocate defs *)
      (*
      It is important that we do the defs first before releasing dead defs instead
      of just calculating the set different so we can check that we spilled properly
    *)
      let defs = Instr.iter_defs instr'.i |> Iter.to_list in
      begin
        let@: def = List.iter defs in
        let color =
          Set.min_elt !available_colors
          |> Option.value_exn ~message:"No colors left, but we should have pre spilled"
        in
        available_colors := Set.remove !available_colors color;
        allocation.!(def) <- color
      end;
      (* release dead defs *)
      (* TODO: maybe check that the color is in the usable ones *)
      begin
        let@: def =
          List.iter defs |> Iter.filter ~f:(fun def -> not (Table.mem use_locations def))
        in
        if Set.mem usable_colors allocation.!(def)
        then available_colors := Set.add !available_colors allocation.!(def)
      end
    end
  end
;;

let alloc_func ~colors func =
  let pred_table = Func.pred_table func in
  let live_in_table, live_out_table = Liveness.compute ~pred_table func in
  let labels = Func.labels_postorder func in
  let allocation = Table.create () in
  let _, use_locations = Liveness.compute_def_use_labels func in
  begin
    let@: label = Vec.iter_rev labels in
    let block = Func.find_block_exn func label in
    let live_in = Liveness.Live_set.find live_in_table label in
    let live_out = Liveness.Live_set.find live_out_table label in
    let is_start = Label.equal label func.start in
    alloc_block
      ~is_start
      ~use_locations
      ~usable_colors:colors
      ~live_in
      ~live_out
      ~allocation
      block
  end;
  allocation
;;
