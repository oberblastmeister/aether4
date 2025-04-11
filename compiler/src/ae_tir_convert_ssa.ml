open Std
open Ae_trace
open Ae_tir_types
module Entity = Ae_entity_std
module Ident = Entity.Ident
module Entity_graph_utils = Ae_entity_graph_utils
module Dominators = Ae_dominators
module Id_gen = Entity.Id_gen
module Bitvec = Entity.Bitvec

open struct
  let is_on_top table ~equal ~key ~data =
    Ident.Table.find_multi table key |> List.hd |> Option.equal equal (Some data)
  ;;
end

let compute_phi_placements func =
  let module Table = Entity.Ident.Table in
  let succ_table = Func.succ_table func in
  let pred_table = Func.pred_table_of_succ succ_table in
  let def_blocks = Liveness.compute_def_blocks_non_ssa func in
  let live_in, _live_out = Liveness.compute_non_ssa ~pred_table func in
  let graph = Entity_graph_utils.bi_graph_of_adj_table ~succ_table ~pred_table in
  let idoms = Func.compute_idoms ~graph func in
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
        Liveness.Live_set.find live_in l |> Fn.flip Ident.Set.mem def)
    in
    if not (is_on_top label_to_phis ~equal:Temp.equal ~key:def_frontier ~data:def)
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

let convert (func : Func.t) =
  let module Table = Entity.Ident.Table in
  let open Table.Syntax in
  let idoms = Func.compute_idoms func in
  let dom_tree = Dominators.Tree.of_immediate idoms in
  let def_to_ty = Func.get_ty_table func in
  let block_to_phis = compute_phi_placements func in
  let temp_gen = Id_gen.create () in
  let multi_edit = Multi_edit.create () in
  let rec rename_block rename_temp_map (block : Block.t) =
    let rename_temp_map = ref rename_temp_map in
    let find_renamed_temp temp =
      Ident.Map.find !rename_temp_map temp
      |> Option.value_exn
           ~error:
             (Error.create
                "Temporary was not initialized on all code paths. The input program must \
                 be strict, which means each temporary must be initialized on all paths \
                 before used. This allows the result to be in strict-ssa form"
                temp
                [%sexp_of: Temp.t])
    in
    begin
      let@: instr = Block.iter_fwd block in
      let instr =
        let@: instr = Instr'.map instr in
        let instr =
          match instr with
          | Block_params params ->
            let phis = Table.find_multi block_to_phis block.label in
            let params =
              params
              @ List.map phis ~f:(fun param ->
                { Block_param.param; ty = def_to_ty.!(param) })
            in
            Instr.Block_params params
          | _ -> instr
        in
        let instr =
          let@: temp = Instr.map_uses instr in
          find_renamed_temp temp
        in
        let instr =
          let@: temp = Instr.map_defs instr in
          let new_temp = Ident.freshen temp_gen temp in
          rename_temp_map := Ident.Map.set !rename_temp_map ~key:temp ~data:new_temp;
          new_temp
        in
        let instr =
          let@: block_call = Instr.map_block_calls instr in
          let new_args =
            Table.find_multi block_to_phis block_call.label
            |> List.map ~f:find_renamed_temp
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
      rename_block rename_temp_map (Ident.Map.find_exn func.blocks label)
    end
  in
  rename_block Ident.Map.empty (Func.start_block func);
  { func with
    blocks =
      func.blocks
      (*
         The unreachable blocks were not in the dominator tree, so were not converted at all.
        Just remove them here.
      *)
      |> Ident.Map.filteri ~f:(fun ~key ~data:_ -> Dominators.is_reachable idoms key)
      |> Multi_edit.apply_blocks ~no_sort:() multi_edit
  ; next_temp_id = Id_gen.next temp_gen
  }
;;
