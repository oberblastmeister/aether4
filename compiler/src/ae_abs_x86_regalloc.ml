open Std
open Ae_abs_x86_types
module Entity = Ae_entity_std
module Id = Entity.Id
module Ident = Entity.Ident
module Frame = Ae_x86_frame
module Int_table = Entity.Table.Int_table
module Bitset = Ae_data_bitset
module Regalloc = Ae_graph_greedy_regalloc
module Graph = Regalloc.Graph
module Mach_reg = Ae_x86_mach_reg
module Id_gen = Entity.Id_gen
module Call_conv = Ae_x86_call_conv
module Chaos_mode = Ae_chaos_mode
open Ae_trace

let build_graph_instr ~get_precolored_name ~graph ~live_out ~(instr : Instr'.t) =
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
    fun live -> not @@ List.mem ~equal:Vreg.equal currently_defining live
  in
  (* add interference edges *)
  begin
    let@: live = Ident.Set.iter live_out |> Iter.filter ~f:can_add_edge_to |> Iter.iter in
    List.iter defs |> Iter.iter ~f:(fun def -> Graph.add_edge graph def live);
    begin
      let@: mach_reg = Instr.iter_mach_reg_defs instr.i in
      let precolored_name = get_precolored_name mach_reg in
      Graph.add_edge graph precolored_name live
    end
  end;
  (*
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
      let precolored_name = get_precolored_name mach_reg in
      begin
        let@: vreg = Ae_x86_address.iter_regs addr in
        Graph.add_edge graph precolored_name vreg
      end
    end
    | _ -> ()
  end;
  Liveness.backwards_transfer instr.i live_out
;;

let build_graph_block ~get_precolored_name ~graph ~live_out ~(block : Block.t) =
  let live_out = ref live_out in
  begin
    let@: instr = Block.iter_bwd block in
    live_out := build_graph_instr ~get_precolored_name ~graph ~live_out:!live_out ~instr
    (* trace_s [%message (live_out : Vreg.Set.t ref)] *)
  end
;;

let build_graph (func : Func.t) =
  (* trace_s [%message "build_graph" (func : Func.t)]; *)
  let open Ident.Table.Syntax in
  let graph = Graph.create () in
  let mach_reg_to_precolored_name = Hashtbl.create (module Mach_reg) in
  assert (not (Graph.mem graph (Ident.create "next_temp_id" func.next_temp_id)));
  let precolored_id_start = func.next_temp_id in
  let temp_gen = Id_gen.of_id func.next_temp_id in
  let find_precolored_name_or_add mach_reg =
    Hashtbl.find_or_add mach_reg_to_precolored_name mach_reg ~default:(fun () ->
      let precolored_name =
        Ident.fresh ~name:(Sexp.to_string (Mach_reg.sexp_of_t mach_reg)) temp_gen
      in
      Graph.add graph precolored_name;
      precolored_name)
  in
  (* TODO: do ssa conversion here *)
  let _live_in, live_out =
    Liveness.compute_non_ssa ~pred_table:(Func.pred_table func) func
  in
  (* trace_s [%message (live_in : Liveness.Live_set.t) (live_out : Liveness.Live_set.t)]; *)
  begin
    let@: block = Func.iter_blocks func in
    build_graph_block
      ~get_precolored_name:find_precolored_name_or_add
      ~graph
      ~live_out:(Liveness.Live_set.find live_out block.label)
      ~block
  end;
  let func = { func with next_temp_id = Id_gen.next temp_gen } in
  graph, mach_reg_to_precolored_name, precolored_id_start, func.next_temp_id, func
;;

(* TODO: add in callee saved later *)
let usable_mach_regs : Mach_reg.t list = Call_conv.caller_saved_without_r11

let get_spilled_colors ~precolored_colors ~max_color ~num_regs =
  let chaos_options = Chaos_mode.get_options () in
  let spilled_colors = Bitset.create ~size:(max_color + 1) () in
  begin
    match chaos_options.spill_mode with
    | None ->
      let amount_to_spill = max_color + 1 - num_regs in
      begin
        let amount_to_spill = ref amount_to_spill in
        let color = ref max_color in
        while !amount_to_spill > 0 do
          if not (Bitset.mem precolored_colors !color)
          then begin
            Bitset.add spilled_colors !color;
            decr amount_to_spill
          end;
          decr color
        done
      end
    | Some All ->
      let color = ref max_color in
      while !color >= 0 do
        if not (Bitset.mem precolored_colors !color)
        then begin
          Bitset.add spilled_colors !color
        end;
        decr color
      done
    | Some Random -> todol [%here]
  end;
  spilled_colors
;;

let spill_instr
      ~spilled_temp_to_slot
      ~allocation
      ~get_evicted_temp_and_slot_for_mach_reg
      ~(instr : Instr'.t)
  =
  let module Table = Ident.Table in
  let open Table.Syntax in
  let evicted_temps = ref [] in
  let used_mach_regs =
    Instr.iter_defs instr.i
    |> Iter.append (Instr.iter_uses instr.i)
    |> Iter.filter_map ~f:(Table.find allocation)
    |> Iter.append (Instr.iter_mach_reg_defs instr.i)
    |> Iter.to_list
    |> Set.of_list (module Mach_reg)
  in
  let ins ?info = Instr'.create_unindexed ?info:(Option.first_some info instr.info) in
  let evict_some_mach_reg =
    (*
       There are always enough evictable colors.
      There are a maxmium of 6 vregs used per instruction.
      Assume that each vreg was allocated a different machine register.
      Then each spilled vreg is one vreg that wasn't used.
      We also have to add two due to the precolored registers.
    *)
    let evictable_mach_regs =
      usable_mach_regs
      |> List.filter ~f:(fun reg -> not (Set.mem used_mach_regs reg))
      |> Lstack.of_list_rev
    in
    fun () ->
      let evicted_mach_reg =
        Lstack.pop evictable_mach_regs
        |> Option.value_exn
             ~error:
               (Error.create "There were no evictable colors left" instr Instr'.sexp_of_t)
      in
      let evicted_temp, evicted_temp_slot =
        get_evicted_temp_and_slot_for_mach_reg evicted_mach_reg
      in
      evicted_temps
      := (evicted_temp, evicted_temp_slot, evicted_mach_reg) :: !evicted_temps;
      evicted_temp, evicted_temp_slot, evicted_mach_reg
  in
  let instrs_before = Lstack.create () in
  let instrs_after = Lstack.create () in
  let spill_info =
    Option.value instr.info ~default:(Info.create_s [%message ""])
    |> Info.tag_s ~tag:[%message "spill"]
  in
  let did_spill = ref false in
  let instr =
    let@: temp =
      (Instr'.map
       & Instr.map_uses
       & Traverse.filtered ~f:(fun temp ->
         (* not in allocation means spilled *)
         not (Table.mem allocation temp)))
        instr
    in
    did_spill := true;
    let evicted_temp, evicted_temp_slot, evicted_mach_reg = evict_some_mach_reg () in
    let temp_slot = spilled_temp_to_slot temp in
    begin
      let info =
        Info.tag_s spill_info ~tag:[%message "evict" (evicted_mach_reg : Mach_reg.t)]
      in
      let ins = ins ~info in
      Lstack.append_list
        instrs_before
        [ ins
            (Mov
               { dst = Stack_slot evicted_temp_slot
               ; src = Reg evicted_temp
               ; size = Qword
               })
        ; ins (Mov { dst = Reg evicted_temp; src = Stack_slot temp_slot; size = Qword })
        ]
    end;
    evicted_temp
  in
  let instr =
    let@: temp =
      (Instr'.map
       & Instr.map_defs
       & Traverse.filtered ~f:(fun temp ->
         (* not in allocation means spilled *)
         not (Table.mem allocation temp)))
        instr
    in
    did_spill := true;
    let evicted_temp, evicted_temp_slot, evicted_mach_reg = evict_some_mach_reg () in
    let temp_slot = spilled_temp_to_slot temp in
    begin
      let info =
        Info.tag_s spill_info ~tag:[%message "evict" (evicted_mach_reg : Mach_reg.t)]
      in
      let ins = ins ~info in
      Lstack.append_list
        instrs_before
        Instr.
          [ ins
              (Mov
                 { dst = Stack_slot evicted_temp_slot
                 ; src = Reg evicted_temp
                 ; size = Qword
                 })
          ];
      Lstack.append_list
        instrs_after
        [ ins (Mov { dst = Stack_slot temp_slot; src = Reg evicted_temp; size = Qword }) ]
    end;
    evicted_temp
  in
  let instr =
    { instr with
      info =
        (if !did_spill
         then Option.map instr.info ~f:(Info.tag ~tag:"spilled")
         else instr.info)
    }
  in
  (* reload all the evicted registers *)
  begin
    let@: evicted_temp, evicted_temp_slot, evicted_mach_reg = List.iter !evicted_temps in
    let info =
      Info.tag_s spill_info ~tag:[%message "evict" (evicted_mach_reg : Mach_reg.t)]
    in
    let ins = ins ~info in
    Lstack.append_list
      instrs_after
      [ ins
          (Mov
             { dst = Reg evicted_temp; src = Stack_slot evicted_temp_slot; size = Qword })
      ]
  end;
  ( Lstack.to_list instrs_before |> List.map ~f:(fun i -> { i with index = instr.index })
  , instr
  , Lstack.to_list instrs_after
    |> List.map ~f:(fun i -> { i with index = instr.index + 1 }) )
;;

(*
   invariant: 

   this must be only called after ssa destruction.
   so the resulting func is not in ssa form.
   this will insert instructions that redefine temporaries.
   
   every thing that is not spilled must be present in allocation.
   
   TODO:
   the spilling is very wrong.
   we can only evict registers that are **not currently used**,
   while we just pick the first available register
*)
let spill_colors ~frame_builder ~allocation ~spilled_colors ~coloring ~(func : Func.t) =
  let module Table = Ident.Table in
  let open Table.Syntax in
  let spilled_temp_to_slot =
    let spilled_color_to_slot = Int_table.create () in
    begin
      let@: spilled_color = Bitset.iter spilled_colors in
      Int_table.set
        spilled_color_to_slot
        ~key:spilled_color
        ~data:
          (Frame.Builder.alloc
             ~name:("spill" ^ Int.to_string spilled_color)
             frame_builder
             Qword)
    end;
    fun temp -> Int_table.find_exn spilled_color_to_slot coloring.!(temp)
  in
  let temp_gen = Id_gen.of_id func.next_temp_id in
  let mach_reg_to_evicted_temp = Hashtbl.create (module Mach_reg) in
  let edit = Multi_edit.create () in
  let evicted_mach_reg_temps = Lstack.create () in
  begin
    let@: block = Func.iter_blocks func in
    let@: instr = Block.iter_fwd block in
    let open Ident.Table.Syntax in
    let instrs_before, new_instr, instrs_after =
      (* just to make sure we don't create too many temporaries *)
      let get_evicted_temp_and_slot_for_mach_reg mach_reg =
        Hashtbl.find_or_add mach_reg_to_evicted_temp mach_reg ~default:(fun () ->
          let temp = Ident.fresh ~name:"evicted" temp_gen in
          let stack_slot = Frame.Builder.alloc ~name:"evicted_slot" frame_builder Qword in
          Lstack.push evicted_mach_reg_temps (temp, mach_reg);
          temp, stack_slot)
      in
      spill_instr
        ~spilled_temp_to_slot
        ~allocation
        ~get_evicted_temp_and_slot_for_mach_reg
        ~instr
    in
    Multi_edit.add_inserts edit block.label instrs_before;
    Multi_edit.add_replace edit block.label new_instr;
    Multi_edit.add_inserts edit block.label instrs_after;
    ()
  end;
  ( `func
      { func with
        next_temp_id = Id_gen.next temp_gen
      ; blocks = Multi_edit.apply_blocks edit func.blocks
      }
  , `evicted_mach_reg_temps (Lstack.to_list_rev evicted_mach_reg_temps) )
;;

let get_precolored_colors
      ~precolored_range:(precolored_id_start, precolored_id_end)
      ~coloring
      ~max_color
  =
  let precolored_colors = Bitset.create ~size:(max_color + 1) () in
  for precolored_id = Id.to_int precolored_id_start to Id.to_int precolored_id_end - 1 do
    let color = Ident.Table.find_int_exn coloring precolored_id in
    Bitset.add precolored_colors color
  done;
  precolored_colors
;;

module Allocation = struct
  type t = Mach_reg.t Vreg.Table.t [@@deriving sexp_of]

  let find_exn = Ident.Table.find_exn
end

let alloc_func frame_builder (func : Func.t) =
  let open Ident.Table.Syntax in
  let graph, mach_reg_to_precolored_name, precolored_id_start, precolored_id_end, func =
    build_graph func
  in
  let precolored_name_to_mach_reg =
    Hashtbl.to_alist mach_reg_to_precolored_name
    |> List.map ~f:(fun (mach_reg, precolored_name) -> precolored_name, mach_reg)
    |> Hashtbl.of_alist_exn (module Vreg)
  in
  let coloring, max_color =
    let precolored =
      Hashtbl.to_alist mach_reg_to_precolored_name
      |> List.map ~f:(fun (_, vreg) -> vreg)
      |> Ident.Set.of_list_exn
    in
    Regalloc.color_graph graph precolored
  in
  let precolored_colors =
    get_precolored_colors
      ~precolored_range:(precolored_id_start, precolored_id_end)
      ~coloring
      ~max_color
  in
  let spilled_colors =
    get_spilled_colors
      ~precolored_colors
      ~max_color
      ~num_regs:(List.length usable_mach_regs)
  in
  let func = Split_critical.split func in
  let scratch_temp, func, max_color =
    let temp_gen = Id_gen.of_id func.next_temp_id in
    let scratch_temp = Ident.fresh ~name:"par_move_scratch" temp_gen in
    coloring.!(scratch_temp) <- max_color + 1;
    Hashtbl.set precolored_name_to_mach_reg ~key:scratch_temp ~data:R11;
    Hashtbl.set mach_reg_to_precolored_name ~key:R11 ~data:scratch_temp;
    scratch_temp, { func with next_temp_id = Id_gen.next temp_gen }, max_color + 1
  in
  let func =
    Destruct_ssa.destruct
      ~in_same_reg:(fun t1 t2 -> coloring.!(t1) = coloring.!(t2))
      ~get_scratch:(fun () -> scratch_temp)
      func
  in
  let color_to_mach_reg : Mach_reg.t Int_table.t = Int_table.create () in
  (* assign registers to the colors of precolored *)
  let find_usable_mach_reg_and_use_up =
    let used_mach_regs = Hash_set.create (module Mach_reg) in
    (* first color all the precolored ones *)
    begin
      let@: precolored = Hashtbl.iter_keys precolored_name_to_mach_reg in
      let color = coloring.!(precolored) in
      let mach_reg = Hashtbl.find_exn precolored_name_to_mach_reg precolored in
      Hash_set.add used_mach_regs mach_reg;
      Int_table.set color_to_mach_reg ~key:color ~data:mach_reg
    end;
    fun () ->
      let mach_reg =
        List.find usable_mach_regs ~f:(fun usable_mach_reg ->
          not (Hash_set.mem used_mach_regs usable_mach_reg))
        |> Option.value_exn
             ~message:
               "Should have spilled registers so that we will always have enough machine \
                registers"
      in
      Hash_set.add used_mach_regs mach_reg;
      mach_reg
  in
  assert (max_color >= -1);
  begin
    let@: color =
      (* assign registers to colors *)
      Iter.Infix.(0 -- max_color)
      (* skip the precolored colors *)
      |> Iter.filter ~f:(fun color -> not @@ Int_table.mem color_to_mach_reg color)
      (* skip the spilled colors *)
      |> Iter.filter ~f:(fun color -> not @@ Bitset.mem spilled_colors color)
    in
    let mach_reg = find_usable_mach_reg_and_use_up () in
    Int_table.set color_to_mach_reg ~key:color ~data:mach_reg
  end;
  let allocation = Ident.Table.create () in
  (* set the colored ones *)
  begin
    let@: temp, color = Ident.Table.iteri coloring in
    match Int_table.find color_to_mach_reg color with
    | None -> assert (Bitset.mem spilled_colors color)
    | Some mach_reg -> allocation.!(temp) <- mach_reg
  end;
  let `func func, `evicted_mach_reg_temps evicted_mach_reg_temps =
    spill_colors ~frame_builder ~allocation ~spilled_colors ~coloring ~func
  in
  (* now set the evicted temps *)
  begin
    let@: evicted_mach_reg_temp, evicted_mach_reg = List.iter evicted_mach_reg_temps in
    allocation.!(evicted_mach_reg_temp) <- evicted_mach_reg
  end;
  allocation, func
;;
