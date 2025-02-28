open Std
module Tir = Ae_tir_types
module Entity = Ae_entity_std
module Ident = Entity.Ident
module Entity_graph_utils = Ae_entity_graph_utils
module Dominators = Ae_dominators
module Id_gen = Entity.Id_gen
module Bitset = Entity.Bitset

open struct
  let is_on_top table ~equal ~key ~data =
    Ident.Table.find_multi table key |> List.hd |> Option.equal equal (Some data)
  ;;
end

let compute_phi_placements func =
  let module Table = Entity.Ident.Table in
  let open Table.Syntax in
  let succ_table = Tir.Func.succ_table func in
  let pred_table = Tir.Func.pred_table_of_succ succ_table in
  let def_blocks = Tir.Liveness.compute_def_blocks func in
  let live_in, _live_out = Tir.Liveness.compute_non_ssa ~pred_table func in
  let graph = Entity_graph_utils.bi_graph_of_adj_table ~succ_table ~pred_table in
  let idoms = Tir.Func.compute_idoms ~graph func in
  let dom_front = Dominators.Frontier.compute idoms graph in
  let label_to_phis = Table.create () in
  begin
    let@: def, def_in_labels = Table.iteri def_blocks in
    let work_stack = Stack.of_list def_in_labels in
    let@: def_in_label = Iter.while_some (fun () -> Stack.pop work_stack) in
    let@: def_frontier =
      Dominators.Frontier.find_iter dom_front def_in_label
      |> Iter.filter ~f:(fun l ->
        (* we only want to insert phis where the variable is actually *live* *)
        Table.find live_in l
        |> Option.value ~default:Ident.Set.empty
        |> Fn.flip Ident.Set.mem def)
    in
    if not (is_on_top label_to_phis ~equal:Tir.Temp.equal ~key:def_frontier ~data:def)
    then begin
      Table.add_multi label_to_phis ~key:def_frontier ~data:def;
      (* We are adding a phi to this block, *)
      (* this itself counts as a definition of the variable def *)
      (* so add it to the work list *)
      (* This is what it means to be the *iterated* frontier *)
      Stack.push work_stack def_frontier
    end
  end;
  label_to_phis
;;

let convert (func : Tir.Func.t) =
  let module Table = Entity.Ident.Table in
  let open Table.Syntax in
  let idoms = Tir.Func.compute_idoms func in
  let dom_tree = Dominators.Tree.of_immediate idoms in
  let block_to_phis = compute_phi_placements func in
  let temp_gen = Id_gen.of_id func.next_temp_id in
  let multi_edit = Tir.Multi_edit.create () in
  let rec rename_block rename_temp_map (block : Tir.Block.t) =
    let rename_temp_map = ref rename_temp_map in
    let find_renamed_temp temp =
      Ident.Map.find !rename_temp_map temp
      |> Option.value_exn
           ~error:
             (Error.create
                "Temporary was not initialized on all code paths"
                temp
                [%sexp_of: Tir.Temp.t])
    in
    begin
      let@: instr = Tir.Block.iter_fwd block in
      let instr =
        let@: instr = Tir.Instr'.map instr in
        let instr =
          match instr with
          | BlockParams { temps } ->
            let phis = Table.find_multi block_to_phis block.label in
            (* TODO: make sure the definition has the proper type using a type map *)
            let temps = temps @ List.map phis ~f:(fun temp -> temp, Tir.Ty.Int) in
            Tir.Instr.BlockParams { temps }
          | _ -> instr
        in
        let instr =
          let@: temp = Tir.Instr.map_uses instr in
          find_renamed_temp temp
        in
        let instr =
          let@: temp = Tir.Instr.map_defs instr in
          let new_temp = Ident.freshen temp_gen temp in
          rename_temp_map := Ident.Map.set !rename_temp_map ~key:temp ~data:new_temp;
          new_temp
        in
        let instr =
          let@: block_call = Tir.Instr.map_block_calls instr in
          let new_args =
            Table.find_multi block_to_phis block_call.label
            |> List.map ~f:find_renamed_temp
          in
          { block_call with args = block_call.args @ new_args }
        in
        instr
      in
      Tir.Multi_edit.add_replace multi_edit block.label instr
    end;
    let rename_temp_map = !rename_temp_map in
    begin
      let@: label = Dominators.Tree.children dom_tree block.label |> List.iter in
      rename_block rename_temp_map (Ident.Map.find_exn func.blocks label)
    end
  in
  rename_block Ident.Map.empty (Tir.Func.start_block func);
  { func with
    blocks = Tir.Multi_edit.apply_blocks ~no_sort:() multi_edit func.blocks
  ; next_temp_id = Id_gen.next temp_gen
  }
;;
