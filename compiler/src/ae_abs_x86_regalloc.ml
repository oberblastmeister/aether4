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
open Ae_trace

let mach_reg_id off mach_reg = Id.offset off (Mach_reg.to_enum mach_reg)

let mach_reg_ident ?info off mach_reg =
  let id = mach_reg_id off mach_reg in
  Ident.create ?info (Mach_reg.to_string mach_reg) id
;;

let[@inline] build_graph_instr ~graph ~live_out ~(instr : Instr'.t) =
  let iter_pairs xs ~f =
    let rec go xs =
      match xs with
      | [] -> ()
      | x :: xs ->
        List.iter xs ~f:(fun y -> f (x, y));
        go xs
    in
    go xs
  in
  let defs = Instr.iter_defs instr.i |> Iter.to_list in
  (* make sure we at least add every use/def in, because the register allocator uses the domain of interference as all nodes *)
  begin
    let@: def = List.iter defs in
    Graph.add graph def
  end;
  (* ensure that multiple defs interfere with each other *)
  iter_pairs defs ~f:(fun (def1, def2) -> Graph.add_edge graph def1 def2);
  let can_add_edge_to =
    let currently_defining = defs in
    fun live -> not @@ List.mem ~equal:Temp.equal currently_defining live
  in
  (* add interference edges *)
  begin
    let@: live = Ident.Set.iter live_out |> Iter.filter ~f:can_add_edge_to |> Iter.iter in
    List.iter defs |> Iter.iter ~f:(fun def -> Graph.add_edge graph def live)
    (* begin
      let@: mach_reg = Instr.iter_mach_reg_defs instr.i in
      let precolored_temp = Mach_reg_gen.get mach_reg_gen mach_reg in
      Graph.add_edge graph precolored_temp live
    end *)
  end;
  (* (*
     for special instructions that take memory destination but also implicitly write registers such as RAX or RDX,
     we must prevent the dst operand from being allocated RAX or RDX or else it will be clobbered.
     
     This is because above, we only add edges to things that are live_out at this instruction.
     However, any memory operands used by the instruction we use *after* we write to the precolored register.
     This means that they are always live_out with respect to the precolored register, even if they may not be in the program.
  *)
  begin
    match instr.i with
    | Instr.Bin { dst = Mem addr; _ } -> begin
      let@: mach_reg = Instr.iter_mach_reg_defs instr.i in
      let precolored_temp = Mach_reg_gen.get mach_reg_gen mach_reg in
      begin
        let@: temp = Ae_x86_address.iter_regs addr in
        Graph.add_edge graph precolored_temp temp
      end
    end
    | _ -> ()
  end; *)
  Liveness.backwards_transfer instr.i live_out
;;

let build_graph_block ~graph ~live_out ~(block : Block.t) =
  let live_out = ref live_out in
  begin
    let@: instr = Block.iter_bwd block in
    live_out := build_graph_instr ~graph ~live_out:!live_out ~instr
  end
;;

let build_graph (func : Func.t) =
  let open Ident.Table.Syntax in
  let graph = Graph.create () in
  let _live_in, live_out = Liveness.compute ~pred_table:(Func.pred_table func) func in
  begin
    let@: block = Func.iter_blocks func in
    build_graph_block
      ~graph
      ~live_out:(Liveness.Live_set.find live_out block.label)
      ~block
  end;
  graph
;;

let alloc_func (func : Func.t) =
  let available_colors =
    List.map Call_conv.regalloc_usable_mach_regs ~f:Mach_reg.to_enum |> Int.Set.of_list
  in
  let open Ident.Table.Syntax in
  let graph = build_graph func in
  let allocation, used_colors =
    (* let precolored =
      Hashtbl.to_alist mach_reg_gen.table
      |> List.map ~f:(fun (mach_reg, temp) -> temp, Mach_reg.to_enum mach_reg)
      |> Ident.Map.of_alist_exn
    in *)
    let precolored = Ident.Map.empty in
    Regalloc.color_graph ~spilled_color:Mach_reg.num ~available_colors ~graph ~precolored
  in
  let spilled_colors = Set.diff used_colors available_colors in
  allocation, spilled_colors
;;
