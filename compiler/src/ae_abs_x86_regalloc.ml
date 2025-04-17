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

let[@inline] build_graph_instr ~mach_reg_id ~graph ~live_out ~(instr : Instr'.t) =
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
    List.iter defs |> Iter.iter ~f:(fun def -> Graph.add_edge graph def live);
    begin
      let@: mach_reg = Instr.iter_mach_reg_defs instr.i in
      let precolored_name = mach_reg_ident mach_reg_id mach_reg in
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
      let precolored_name = mach_reg_ident mach_reg_id mach_reg in
      begin
        let@: temp = Ae_x86_address.iter_regs addr in
        Graph.add_edge graph precolored_name temp
      end
    end
    | _ -> ()
  end;
  Liveness.backwards_transfer instr.i live_out
;;

let build_graph_block ~mach_reg_id ~graph ~live_out ~(block : Block.t) =
  let live_out = ref live_out in
  begin
    let@: instr = Block.iter_bwd block in
    live_out := build_graph_instr ~mach_reg_id ~graph ~live_out:!live_out ~instr
  end
;;

let build_graph ~mach_reg_id (func : Func.t) =
  let open Ident.Table.Syntax in
  let graph = Graph.create () in
  let _live_in, live_out = Liveness.compute ~pred_table:(Func.pred_table func) func in
  begin
    let@: block = Func.iter_blocks func in
    build_graph_block
      ~mach_reg_id
      ~graph
      ~live_out:(Liveness.Live_set.find live_out block.label)
      ~block
  end;
  graph, func
;;

module Allocation = struct
  type t =
    { table : int Temp.Table.t
    ; mach_reg_id : Temp_entity.Id.t
    }
  [@@deriving sexp_of]

  let find_color_exn t (temp : Temp.t) =
    let mach_reg_id = (t.mach_reg_id :> int) in
    let temp_id = (temp.id :> int) in
    if temp_id >= mach_reg_id && temp_id < mach_reg_id + Mach_reg.num
    then temp_id - mach_reg_id
    else begin
      let color = Ident.Table.find_exn t.table temp in
      color
    end
  ;;

  let find_exn t (temp : Temp.t) = Mach_reg.of_enum_exn (find_color_exn t temp)
end

let spill_instr
      ~spilled_temp_to_slot
      ~spilled_colors
      ~allocation
      ~get_evicted_temp_and_slot_for_mach_reg
      ~(instr : Instr'.t)
  =
  let module Table = Ident.Table in
  let open Table.Syntax in
  let evicted_temps = ref [] in
  let used_non_spilled_colors =
    Instr.iter_defs instr.i
    |> Iter.append (Instr.iter_uses instr.i)
    |> Iter.map ~f:(fun temp -> Allocation.find_color_exn allocation temp)
    |> Iter.filter ~f:(fun color -> not (Set.mem spilled_colors color))
    |> Iter.append (Instr.iter_mach_reg_defs instr.i |> Iter.map ~f:Mach_reg.to_enum)
    |> Iter.to_list
    |> Int.Set.of_list
  in
  let ins ?info = Instr'.create_unindexed ?info:(Option.first_some info instr.info) in
  let evict_some_mach_reg =
    (*
       There are always enough evictable colors.
      There are a maxmium of 6 temps used per instruction.
      Assume that each temp was allocated a different machine register.
      Then each spilled temp is one temp that wasn't used.
      We also have to add two due to the precolored registers.
    *)
    let evictable_colors =
      Call_conv.regalloc_usable_mach_regs
      |> List.map ~f:Mach_reg.to_enum
      |> List.filter ~f:(fun reg -> not (Set.mem used_non_spilled_colors reg))
      |> Lstack.of_list_rev
    in
    fun () ->
      let evicted_mach_reg =
        Lstack.pop evictable_colors
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
         Set.mem spilled_colors (Allocation.find_color_exn allocation temp)))
        instr
    in
    did_spill := true;
    let evicted_temp, evicted_temp_slot, evicted_mach_reg = evict_some_mach_reg () in
    let temp_slot = spilled_temp_to_slot temp in
    begin
      let info =
        Info.tag_s
          spill_info
          ~tag:
            [%message
              "evict"
                ~evicted_mach_reg:(Mach_reg.of_enum_exn evicted_mach_reg : Mach_reg.t)]
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
         Set.mem spilled_colors (Allocation.find_color_exn allocation temp)))
        instr
    in
    did_spill := true;
    let evicted_temp, evicted_temp_slot, evicted_mach_reg = evict_some_mach_reg () in
    let temp_slot = spilled_temp_to_slot temp in
    begin
      let info =
        Info.tag_s
          spill_info
          ~tag:
            [%message
              "evict"
                ~evicted_mach_reg:(Mach_reg.of_enum_exn evicted_mach_reg : Mach_reg.t)]
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
      Info.tag_s
        spill_info
        ~tag:
          [%message
            "evict" ~evicted_mach_reg:(Mach_reg.of_enum_exn evicted_mach_reg : Mach_reg.t)]
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
*)
let spill_colors ~mach_reg_id ~stack_builder ~allocation ~spilled_colors ~(func : Func.t) =
  let module Table = Ident.Table in
  let open Table.Syntax in
  let spilled_temp_to_slot =
    let spilled_color_to_slot = Int_table.create () in
    begin
      let@: spilled_color = Set.iter spilled_colors in
      Int_table.set
        spilled_color_to_slot
        ~key:spilled_color
        ~data:
          (Stack_builder.alloc
             ~name:("spill" ^ Int.to_string spilled_color)
             stack_builder
             Qword)
    end;
    fun temp ->
      Int_table.find_exn spilled_color_to_slot (Allocation.find_color_exn allocation temp)
  in
  let mach_reg_to_evicted_temp = Hashtbl.create (module Int) in
  let edit = Multi_edit.create () in
  begin
    let@: block = Func.iter_blocks func in
    let@: instr = Block.iter_fwd block in
    let open Ident.Table.Syntax in
    let instrs_before, new_instr, instrs_after =
      let get_evicted_temp_and_slot_for_mach_reg mach_reg =
        Hashtbl.find_or_add mach_reg_to_evicted_temp mach_reg ~default:(fun () ->
          let stack_slot = Stack_builder.alloc ~name:"evicted_slot" stack_builder Qword in
          let temp = mach_reg_ident mach_reg_id (Mach_reg.of_enum_exn mach_reg) in
          temp, stack_slot)
      in
      spill_instr
        ~spilled_temp_to_slot
        ~spilled_colors
        ~allocation
        ~get_evicted_temp_and_slot_for_mach_reg
        ~instr
    in
    Multi_edit.add_inserts edit block.label instrs_before;
    Multi_edit.add_replace edit block.label new_instr;
    Multi_edit.add_inserts edit block.label instrs_after;
    ()
  end;
  { func with blocks = Multi_edit.apply_blocks edit func.blocks }
;;

let get_precolored_colors
      ~precolored_range:(precolored_id_start, precolored_id_end)
      ~coloring
      ~max_color
  =
  let precolored_colors = Bitvec.create ~size:(max_color + 1) () in
  for precolored_id = Id.to_int precolored_id_start to Id.to_int precolored_id_end - 1 do
    let color = Ident.Table.find_int_exn coloring precolored_id in
    Bitvec.add precolored_colors color
  done;
  precolored_colors
;;

let alloc_func ~mach_reg_id (func : Func.t) =
  let precolored =
    List.map Call_conv.regalloc_usable_mach_regs ~f:(fun mach_reg ->
      mach_reg_ident mach_reg_id mach_reg, Mach_reg.to_enum mach_reg)
    |> Ident.Map.of_alist_exn
  in
  let available_colors =
    List.map Call_conv.regalloc_usable_mach_regs ~f:Mach_reg.to_enum |> Int.Set.of_list
  in
  let open Ident.Table.Syntax in
  let graph, func = build_graph ~mach_reg_id func in
  let allocation, used_colors =
    Regalloc.color_graph ~spilled_color:Mach_reg.num ~available_colors ~graph ~precolored
  in
  let allocation = { Allocation.table = allocation; mach_reg_id } in
  let spilled_colors = Set.diff used_colors available_colors in
  allocation, spilled_colors, func
;;
