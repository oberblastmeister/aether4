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

let alloc_block ~max_color ~live_out ~active_temps ~coloring block =
  let used_colors =
    Ident.Set.iter active_temps
    |> Iter.map ~f:coloring.!(__)
    |> Iter.to_list
    |> Int.Set.of_list
    |> ref
  in
  let deaths = Liveness.compute_deaths ~live_out block in
  begin
    let@: instr = Block.iter_fwd block in
    begin
      let@: use = Instr.iter_uses instr.i in
      if Ident.Set.mem deaths.@(instr.index) use
      then used_colors := Set.remove !used_colors coloring.!(use)
    end;
    begin
      let@: def = Instr.iter_defs instr.i in
      let available_color =
        Iter.int_range
          ~start:0
          ~stop:(Set.max_elt !used_colors |> Option.value ~default:(-1) |> ( + ) 1)
        |> Iter.find ~f:(fun color -> not (Set.mem !used_colors color))
        |> Option.value_exn
      in
      coloring.!(def) <- available_color;
      max_color := max !max_color available_color
    end
  end;
  ()
;;

let alloc_func func =
  let pred_table = Func.pred_table func in
  let dominators = Func.compute_idoms func in
  let live_in_table, live_out_table = Liveness.compute ~pred_table func in
  let labels = Func.labels_postorder func in
  let block_active_temps = Table.create () in
  block_active_temps.!(func.start) <- Ident.Set.empty;
  let coloring = Table.create () in
  let max_color = ref (-1) in
  begin
    let@: label = Vec.iter_rev labels in
    let block = Func.find_block_exn func label in
    let idom =
      Dominators.find dominators label
      |> Option.value_exn
           ~message:"label should be reachable because we used dfs from the start label"
    in
    let live_in = Liveness.Live_set.find live_in_table label in
    let live_out = Liveness.Live_set.find live_out_table label in
    (* since live_out is the union of all successors live_in, so temps might die in some successor *)
    let active_temps =
      block_active_temps.!(idom)
      |> Ident.Set.to_list
      |> List.filter ~f:(Ident.Set.mem live_in)
      |> Ident.Set.of_list_exn
    in
    alloc_block ~max_color ~live_out ~active_temps ~coloring block
  end;
  coloring, !max_color
;;
