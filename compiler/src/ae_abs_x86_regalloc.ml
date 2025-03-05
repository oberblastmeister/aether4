open Std
open Ae_abs_x86_types
module Use_defs = Ae_abs_x86_use_defs
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

let iter_pairs xs ~f =
  let rec go xs =
    match xs with
    | [] -> ()
    | x :: xs ->
      List.iter xs ~f:(fun y -> f (x, y));
      go xs
  in
  go xs
;;

let build_graph (func : Func.t) =
  let block = Func.start_block func in
  let live_out = ref Ident.Set.empty in
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
  begin
    let@: instr = Block.iter_bwd block in
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
      let@: live =
        Ident.Set.iter !live_out |> Iter.filter ~f:can_add_edge_to |> Iter.iter
      in
      List.iter defs |> Iter.iter ~f:(fun def -> Graph.add_edge graph def live);
      begin
        let@: mach_reg = Instr.iter_mach_reg_defs instr.i in
        let precolored_name = find_precolored_name_or_add mach_reg in
        Graph.add_edge graph precolored_name live
      end
    end;
    live_out := Liveness.backwards_transfer instr.i !live_out;
    (*
       for special instructions that take memory destination but also implicitly write registers such as RAX or RDX,
       we must prevent the dst operand from being allocated RAX or RDX or else it will be clobbered
    *)
    match instr.i with
    | Instr.Bin { dst = Mem addr; op = Idiv | Imul | Imod; _ } ->
      let rax_name = find_precolored_name_or_add RAX in
      let rdx_name = find_precolored_name_or_add RDX in
      begin
        let@: vreg = Ae_x86_address.iter_regs addr in
        Graph.add_edge graph rdx_name vreg;
        Graph.add_edge graph rax_name vreg
      end
    | _ -> ()
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
      stack_builder
      temp_gen
      spilled_color_to_slot
      color_to_global_evicted (* just to make sure we don't create aoo many temporaries *)
      coloring
      edit
      label
      (instr : Instr'.t)
  =
  let module Table = Ident.Table in
  let open Table.Syntax in
  let evicted_temps = ref [] in
  let evict_color color =
    let evicted_temp =
      Int_table.find_or_add color_to_global_evicted color ~default:(fun () ->
        let temp = Ident.fresh ~name:"evicted" temp_gen in
        let stack_slot = Stack_builder.alloc ~name:"evicted" stack_builder Qword in
        coloring.!(temp) <- color;
        temp, stack_slot)
    in
    evicted_temps := evicted_temp :: !evicted_temps;
    evicted_temp
  in
  let instr_index = instr.index in
  let instr =
    let@: instr = Instr'.map instr in
    let instr =
      let@: temp =
        (Instr.map_uses
         & Traverse.filtered (fun temp ->
           Int_table.mem spilled_color_to_slot coloring.!(temp)))
          instr
      in
      let evicted_temp, evicted_temp_slot = evict_color coloring.!(temp) in
      let temp_slot = Int_table.find_exn spilled_color_to_slot coloring.!(temp) in
      Multi_edit.add_inserts
        edit
        label
        [ Instr'.create
            (Mov
               { dst = Stack_slot evicted_temp_slot
               ; src = Reg evicted_temp
               ; size = Qword
               })
            instr_index
        ; Instr'.create
            (Mov { dst = Reg evicted_temp; src = Stack_slot temp_slot; size = Qword })
            instr_index
        ];
      (* use evicted_temp instead of temp_slot *)
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
      Multi_edit.add_inserts
        edit
        label
        [ Instr'.create
            (Mov
               { dst = Stack_slot evicted_temp_slot
               ; src = Reg evicted_temp
               ; size = Qword
               })
            instr_index
        ; Instr'.create
            (* insert the move to the stack slot after the instruction has finished *)
            (Mov { dst = Stack_slot temp_slot; src = Reg evicted_temp; size = Qword })
            (instr_index + 1)
        ];
      evicted_temp
    in
    instr
  in
  begin
    let@: evicted_temp, evicted_temp_slot = List.iter !evicted_temps in
    Multi_edit.add_insert
      edit
      label
      (Instr'.create
         (Mov { dst = Reg evicted_temp; src = Stack_slot evicted_temp_slot; size = Qword })
         (instr_index + 1))
  end;
  Multi_edit.add_replace edit label instr;
  ()
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
    Int_table.Syntax.(
      spilled_color_to_slot.!(spilled_color)
      <- Stack_builder.alloc
           ~name:("spill" ^ Int.to_string spilled_color)
           stack_builder
           Qword)
  end;
  let temp_gen = Id_gen.of_id func.next_temp_id in
  let color_to_evicted = Int_table.create () in
  let edit = Multi_edit.create () in
  begin
    let@: block = Func.iter_blocks func in
    let@: instr = Block.iter_fwd block in
    spill_instr
      stack_builder
      temp_gen
      spilled_color_to_slot
      color_to_evicted
      coloring
      edit
      block.label
      instr
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
  (* let func =
    Destruct_ssa.destruct ~in_same_reg:(fun t1 t2 -> coloring.!(t1) = coloring.!(t2)) func
  in *)
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
  allocation, func
;;
