open Std
open Ae_generic_ir_import

module Make (Ir : Ir) = struct
  open Ir

  open struct
    let is_on_top table ~equal ~key ~data =
      Label.Table.find_multi table key |> List.hd |> Option.equal equal (Some data)
    ;;
  end

  module Liveness = Ae_generic_ir_liveness.Make (Ir)

  let get_num_definitions func =
    let table = Temp.Table.create () in
    begin
      let@: block = Func.iter_blocks func in
      let@: instr = Block.iter_fwd block in
      let@: def = Instr.iter_defs instr.i in
      Temp.Table.update table def ~f:(Option.value_map ~f:(( + ) 1) ~default:1)
    end;
    table
  ;;

  let compute_phi_placements func =
    let succ_table = Func.succ_table func in
    let pred_table = Func.pred_table_of_succ succ_table in
    let def_blocks = Liveness.compute_def_blocks_non_ssa func in
    let live_in, _live_out = Liveness.compute_non_ssa ~pred_table func in
    let graph = Func.bi_graph func in
    let idoms = Func.compute_idoms ~graph func in
    let dom_front = Dominators.Frontier.compute idoms graph in
    let label_to_phis = Label.Table.create () in
    begin
      let@: def, def_in_labels = Temp.Table.iteri def_blocks in
      let work_stack = Stack.of_list def_in_labels in
      let@: def_in_label = Iter.while_some (fun () -> Stack.pop work_stack) in
      let@: def_frontier =
        Dominators.Frontier.find_iter dom_front def_in_label
        |> Iter.filter ~f:(fun l ->
          (*
             we only want to insert phis where the variable is actually *live*,
            this is pruned SSA
          *)
          Liveness.Live_set.find live_in l |> Fn.flip Set.mem def)
      in
      if not (is_on_top label_to_phis ~equal:Temp.equal ~key:def_frontier ~data:def)
      then begin
        Label.Table.add_multi label_to_phis ~key:def_frontier ~data:def;
        (* We are adding a phi to this block, *)
        (* this itself counts as a definition of the variable def *)
        (* so add it to the work list *)
        (* This is what it means to be the *iterated* frontier *)
        Stack.push work_stack def_frontier
      end
    end;
    label_to_phis
  ;;

  let convert ?renumber (func : Func.t) =
    let idoms = Func.compute_idoms func in
    let dom_tree = Dominators.Tree.of_immediate idoms in
    let def_to_ty = Func.get_ty_table func in
    let block_to_phis = compute_phi_placements func in
    let num_definitions = get_num_definitions func in
    let temp_gen =
      if Option.is_some renumber then Temp.Id_gen.create 0 else Func.create_temp_gen func
    in
    let multi_edit = Multi_edit.create () in
    let rec rename_block rename_temp_map (block : Block.t) =
      let rename_temp_map = ref rename_temp_map in
      let find_renamed_temp temp =
        Map.find !rename_temp_map temp
        |> Option.value_exn
             ~error:
               (Error.create
                  "Temporary was not initialized on all code paths. The input program \
                   must be strict, which means each temporary must be initialized on all \
                   paths before used. This allows the result to be in strict-ssa form"
                  temp
                  Temp.sexp_of_t)
      in
      begin
        let@: instr = Block.iter_fwd block in
        let instr =
          let@: instr = Instr'.map instr in
          let instr =
            match instr with
            | _ when Instr.is_block_params instr ->
              let params = Instr.block_params_val instr |> Option.value_exn in
              let phis = Label.Table.find_multi block_to_phis block.label in
              let params =
                params
                @ List.map phis ~f:(fun temp ->
                  { Block_param.param = Location.of_temp temp
                  ; ty = def_to_ty.Temp.Table.Syntax.!(temp)
                  })
              in
              Instr.block_params params
            | _ -> instr
          in
          let instr =
            let@: temp = Instr.map_uses instr in
            find_renamed_temp temp
          in
          let instr =
            let@: temp = Instr.map_defs instr in
            let new_temp =
              (*
                 keep the same name for temporaries that are already ssa,
                these can't possibly create any new phis
              *)
              if Option.is_some renumber || num_definitions.Temp.Table.Syntax.!(temp) > 1
              then Temp.freshen temp_gen temp
              else temp
            in
            rename_temp_map := Map.set !rename_temp_map ~key:temp ~data:new_temp;
            new_temp
          in
          let instr =
            let@: block_call = Instr.map_block_calls instr in
            let new_args =
              Label.Table.find_multi block_to_phis block_call.label
              |> List.map ~f:(fun temp -> Location.of_temp (find_renamed_temp temp))
            in
            { block_call with args = block_call.args @ new_args }
          in
          instr
        in
        Multi_edit.add_replace multi_edit block.label instr
      end;
      let rename_temp_map = !rename_temp_map in
      begin
        let@: label = Dominators.Tree.children dom_tree block.label |> List.iter in
        rename_block rename_temp_map (Map.find_exn (Func.blocks func) label)
      end
    in
    rename_block Temp.Map.empty (Func.start_block func);
    let new_blocks =
      Func.blocks func
      (*
         The unreachable blocks were not in the dominator tree, so were not converted at all.
      Just remove them here.
      *)
      |> Map.filteri ~f:(fun ~key ~data:_ -> Dominators.is_reachable idoms key)
      |> Multi_edit.apply_blocks ~no_sort:() multi_edit
    in
    Func.set_blocks func new_blocks |> Func.apply_temp_gen ?renumber temp_gen
  ;;
end
