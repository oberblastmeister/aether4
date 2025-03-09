open Std
open Ae_abs_x86_types
module Entity = Ae_entity_std
module Id = Entity.Id
module Ident = Entity.Ident
module Stack_builder = Ae_stack_builder
module Int_table = Entity.Table.Int_table
module Bitset = Ae_data_bitset
module Regalloc = Ae_graph_greedy_regalloc
module Graph = Regalloc.Graph
module Mach_reg = Ae_x86_mach_reg
module Id_gen = Entity.Id_gen
module Call_conv = Ae_x86_call_conv
open Ae_trace

let build_graph_instr ~get_precolored_name graph live_out (instr : Instr'.t) =
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
  ( (* let uses = Instr.iter_uses instr.i |> Iter.to_list in *)
    (* trace_s
      [%message
        "build_graph_instr"
          (instr : Instr'.t)
          (live_out : Vreg.Set.t)
          (defs : Vreg.t list)
          (uses : Vreg.t list)] *) );
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

let build_graph_block ~get_precolored_name graph live_out (block : Block.t) =
  (* trace_s [%message "build_graph_block" (block.label : Label.t) (live_out : Vreg.Set.t)]; *)
  let live_out = ref live_out in
  begin
    let@: instr = Block.iter_bwd block in
    live_out := build_graph_instr ~get_precolored_name graph !live_out instr
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
      graph
      (Liveness.Live_set.find live_out block.label)
      block
  end;
  let func = { func with next_temp_id = Id_gen.next temp_gen } in
  graph, mach_reg_to_precolored_name, precolored_id_start, func.next_temp_id, func
;;

(* TODO: add in callee saved later *)
let usable_mach_regs : Mach_reg.t list = Call_conv.caller_saved_without_r11

let get_spilled_colors precolored_colors ~max_color ~num_regs =
  let spilled_colors = Bitset.create ~size:(max_color + 1) () in
  let amount_to_spill = max_color + 1 - num_regs in
  begin
    let amount_to_spill = ref amount_to_spill in
    let color = ref max_color in
    while !amount_to_spill > 0 do
      if Bitset.mem precolored_colors !color
      then begin
        ()
      end
      else begin
        Bitset.add spilled_colors !color;
        decr amount_to_spill
      end;
      decr color
    done
  end;
  spilled_colors
;;

let spill_instr
      spilled_color_to_slot
      get_evicted_temp_and_slot_for_color
      coloring
      (instr : Instr.t)
  =
  let module Table = Ident.Table in
  let open Table.Syntax in
  let evicted_temps = ref [] in
  let evict_color color =
    let evicted_temp = get_evicted_temp_and_slot_for_color color in
    evicted_temps := evicted_temp :: !evicted_temps;
    evicted_temp
  in
  let instrs_before = Lstack.create () in
  let instrs_after = Lstack.create () in
  let instr =
    let instr =
      let@: temp =
        (Instr.map_uses
         & Traverse.filtered (fun temp ->
           Int_table.mem spilled_color_to_slot coloring.!(temp)))
          instr
      in
      let evicted_temp, evicted_temp_slot = evict_color coloring.!(temp) in
      let temp_slot = Int_table.find_exn spilled_color_to_slot coloring.!(temp) in
      Lstack.append_list
        instrs_before
        Instr.
          [ Mov
              { dst = Stack_slot evicted_temp_slot; src = Reg evicted_temp; size = Qword }
          ; Mov { dst = Reg evicted_temp; src = Stack_slot temp_slot; size = Qword }
          ];
      evicted_temp
    in
    let instr =
      let@: temp =
        (Instr.map_defs
         & Traverse.filtered (fun temp ->
           Int_table.mem spilled_color_to_slot coloring.!(temp)))
          instr
      in
      let evicted_temp, evicted_temp_slot = evict_color coloring.!(temp) in
      let temp_slot = Int_table.find_exn spilled_color_to_slot coloring.!(temp) in
      Lstack.append_list
        instrs_before
        Instr.
          [ Mov
              { dst = Stack_slot evicted_temp_slot; src = Reg evicted_temp; size = Qword }
          ];
      Lstack.append_list
        instrs_after
        Instr.[ Mov { dst = Stack_slot temp_slot; src = Reg evicted_temp; size = Qword } ];
      evicted_temp
    in
    instr
  in
  begin
    let@: evicted_temp, evicted_temp_slot = List.iter !evicted_temps in
    Lstack.append_list
      instrs_after
      [ Mov { dst = Reg evicted_temp; src = Stack_slot evicted_temp_slot; size = Qword } ]
  end;
  Lstack.to_list instrs_before, instr, Lstack.to_list instrs_after
;;

(* this must be only called after ssa destruction.
   so the resulting func is not in ssa form.
   this will insert instructions that redefine temporaries.
*)
let spill_colors stack_builder spilled_colors coloring (func : Func.t) =
  let module Table = Ident.Table in
  let open Table.Syntax in
  let spilled_color_to_slot = Int_table.create () in
  begin
    let@: spilled_color = List.iter @@ Bitset.to_list spilled_colors in
    Int_table.set
      spilled_color_to_slot
      ~key:spilled_color
      ~data:
        (Stack_builder.alloc
           ~name:("spill" ^ Int.to_string spilled_color)
           stack_builder
           Qword)
  end;
  let temp_gen = Id_gen.of_id func.next_temp_id in
  let color_to_global_evicted = Int_table.create () in
  let edit = Multi_edit.create () in
  (* trace_s [%message "coloring func" (func : Func.t)]; *)
  (* trace_s [%message "coloring" (coloring : int Vreg.Table.t)]; *)
  begin
    let@: block = Func.iter_blocks func in
    let@: instr = Block.iter_fwd block in
    let open Ident.Table.Syntax in
    let instrs_before, new_instr, instrs_after =
      (* just to make sure we don't create too many temporaries *)
      let get_evicted_temp_and_slot_for_color color =
        Int_table.find_or_add color_to_global_evicted color ~default:(fun () ->
          let temp = Ident.fresh ~name:"evicted" temp_gen in
          let stack_slot = Stack_builder.alloc ~name:"evicted" stack_builder Qword in
          coloring.!(temp) <- color;
          temp, stack_slot)
      in
      spill_instr
        spilled_color_to_slot
        get_evicted_temp_and_slot_for_color
        coloring
        instr.i
    in
    let instr_index = instr.index in
    Multi_edit.add_inserts
      edit
      block.label
      (List.map instrs_before ~f:(fun instr -> Instr'.create instr instr_index));
    Multi_edit.add_replace edit block.label (Instr'.create new_instr instr_index);
    Multi_edit.add_inserts
      edit
      block.label
      (List.map instrs_after ~f:(fun instr -> Instr'.create instr (instr_index + 1)))
  end;
  { func with
    next_temp_id = Id_gen.next temp_gen
  ; blocks = Multi_edit.apply_blocks edit func.blocks
  }
;;

let get_precolored_colors (precolored_id_start, precolored_id_end) coloring max_color =
  let precolored_colors = Bitset.create ~size:(max_color + 1) () in
  for precolored_id = Id.to_int precolored_id_start to Id.to_int precolored_id_end - 1 do
    let color = Ident.Table.find_int_exn coloring precolored_id in
    Bitset.add precolored_colors color
  done;
  precolored_colors
;;

let color_func_and_spill ~num_regs stack_builder (func : Func.t) =
  let open Ident.Table.Syntax in
  let graph, mach_reg_to_precolored_name, precolored_id_start, precolored_id_end, func =
    build_graph func
  in
  (* trace_s [%message (graph : Graph.t)]; *)
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
    get_precolored_colors (precolored_id_start, precolored_id_end) coloring max_color
  in
  let spilled_colors = get_spilled_colors precolored_colors ~max_color ~num_regs in
  let func = Split_critical.split func in
  let scratch_temp, func, max_color =
    let temp_gen = Id_gen.of_id func.next_temp_id in
    let scratch_temp = Ident.fresh ~name:"par_move_scratch" temp_gen in
    coloring.!(scratch_temp) <- max_color + 1;
    Hashtbl.set precolored_name_to_mach_reg ~key:scratch_temp ~data:R11;
    Hashtbl.set mach_reg_to_precolored_name ~key:R11 ~data:scratch_temp;
    scratch_temp, { func with next_temp_id = Id_gen.next temp_gen }, max_color + 1
  in
  let get_scratch () = scratch_temp in
  let func =
    Destruct_ssa.destruct
      ~in_same_reg:(fun t1 t2 -> coloring.!(t1) = coloring.!(t2))
      ~get_scratch
      func
  in
  (* trace_s
    [%message
      "after destruct"
        (coloring : int Vreg.Table.t)
        (func : Func.t)
        (spilled_colors : Bitset.t)]; *)
  let func = spill_colors stack_builder spilled_colors coloring func in
  coloring, max_color, precolored_name_to_mach_reg, func
;;

module Allocation = struct
  type t =
    { color_to_mach_reg : Mach_reg.t Int_table.t
    ; coloring : int Vreg.Table.t
    }

  let sexp_of_t t =
    let coloring = t.coloring in
    let color_to_mach_reg = Int_table.to_list t.color_to_mach_reg in
    [%message (coloring : int Vreg.Table.t) (color_to_mach_reg : (int * Mach_reg.t) list)]
  ;;

  let find_exn t vreg =
    let open Ident.Table.Syntax in
    Int_table.find_exn t.color_to_mach_reg t.coloring.!(vreg)
  ;;
end

let alloc_func stack_builder (func : Func.t) =
  let open Ident.Table.Syntax in
  let coloring, max_color, precolored_name_to_mach_reg, func =
    color_func_and_spill ~num_regs:(List.length usable_mach_regs) stack_builder func
  in
  let color_to_mach_reg : Mach_reg.t Int_table.t = Int_table.create () in
  (* assign registers to the colors of precolored *)
  let find_usable_mach_reg_and_use_up =
    let used_mach_regs = Hash_set.create (module Mach_reg) in
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
    (* TODO: this is wrong, we need to return spilled from color_func_and_spill and only assign registers to non spilled colors *)
    let@: color =
      (* assign registers to colors *)
      Iter.Infix.(0 -- max_color)
      (* skip the precolored colors *)
      |> Iter.filter ~f:(fun color -> not @@ Int_table.mem color_to_mach_reg color)
    in
    let mach_reg = find_usable_mach_reg_and_use_up () in
    Int_table.set color_to_mach_reg ~key:color ~data:mach_reg
  end;
  let allocation = { Allocation.coloring; color_to_mach_reg } in
  (* print_s [%message (allocation : Allocation.t)]; *)
  allocation, func
;;
