open Std
open Ae_abs_x86_types
module Entity = Ae_entity_std
module Id = Entity.Id
module Ident = Entity.Ident
module Frame = Ae_x86_frame
module Int_table = Entity.Table.Int_table
module Bitvec = Ae_data_bitvec
module Regalloc = Ae_graph_greedy_regalloc
module Graph = Regalloc.Graph
module Mach_reg = Ae_x86_mach_reg
module Id_gen = Entity.Id_gen
module Call_conv = Ae_x86_call_conv
module Chaos_mode = Ae_chaos_mode
module Destruct_ssa = Ae_abs_x86_destruct_ssa
open Ae_trace

let get_spilled_colors ~precolored_colors ~max_color ~num_regs =
  let chaos_options = Chaos_mode.get_options () in
  let spilled_colors = Bitvec.create ~size:(max_color + 1) () in
  begin
    match chaos_options.spill_mode with
    | None ->
      let amount_to_spill = max_color + 1 - num_regs in
      begin
        let amount_to_spill = ref amount_to_spill in
        let color = ref max_color in
        while !amount_to_spill > 0 do
          if not (Bitvec.mem precolored_colors !color)
          then begin
            Bitvec.add spilled_colors !color;
            decr amount_to_spill
          end;
          decr color
        done
      end
    | Some All ->
      let color = ref max_color in
      while !color >= 0 do
        if not (Bitvec.mem precolored_colors !color)
        then begin
          Bitvec.add spilled_colors !color
        end;
        decr color
      done
    | Some Random -> todol [%here]
  end;
  spilled_colors
;;

let get_mach_reg_temp_id_start (func : Func.t) =
  func.next_temp_id, { func with next_temp_id = Id.offset func.next_temp_id Mach_reg.num }
;;

let mach_reg_id off mach_reg = Id.offset off (Mach_reg.to_enum mach_reg)

let mach_reg_ident ?info off mach_reg =
  let id = mach_reg_id off mach_reg in
  Ident.create ?info (Mach_reg.to_string mach_reg) id
;;

let usable_mach_regs : Mach_reg.t list = Call_conv.caller_saved_without_r11

let spill_instr
      ~spilled_temp_to_slot
      ~allocation
      ~mach_reg_start
      ~mach_reg_slot_start
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
      There are a maxmium of 6 temps used per instruction.
      Assume that each temp was allocated a different machine register.
      Then each spilled temp is one temp that wasn't used.
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
      let evicted_temp = mach_reg_ident mach_reg_start evicted_mach_reg in
      let evicted_temp_slot = mach_reg_ident mach_reg_slot_start evicted_mach_reg in
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

let spill_func ~num_regs ~allocation ~coloring ~max_color (func : Func.t) =
  let spilled_colors =
    get_spilled_colors ~precolored_colors:(Bitvec.create ()) ~max_color ~num_regs
  in
  let mach_reg_start, mach_reg_slot_start, func =
    ( func.next_temp_id
    , func.data.next_stack_slot_id
    , { func with
        next_temp_id = Id.offset func.next_temp_id Mach_reg.num
      ; data =
          { func.data with
            next_stack_slot_id = Id.offset func.data.next_stack_slot_id Mach_reg.num
          }
      } )
  in
  let module Table = Ident.Table in
  let open Table.Syntax in
  let stack_slot_gen = Id_gen.of_id func.data.next_stack_slot_id in
  let spilled_temp_to_slot =
    let spilled_color_to_slot = Int_table.create () in
    begin
      let@: spilled_color = Bitvec.iter spilled_colors in
      Int_table.set
        spilled_color_to_slot
        ~key:spilled_color
        ~data:(Ident.fresh ~name:("spill" ^ Int.to_string spilled_color) stack_slot_gen)
    end;
    fun temp -> Int_table.find_exn spilled_color_to_slot coloring.!(temp)
  in
  let edit = Multi_edit.create () in
  begin
    let@: block = Func.iter_blocks func in
    let@: instr = Block.iter_fwd block in
    let open Ident.Table.Syntax in
    let instrs_before, new_instr, instrs_after =
      spill_instr
        ~spilled_temp_to_slot
        ~mach_reg_start
        ~mach_reg_slot_start
        ~instr
        ~allocation
    in
    Multi_edit.add_inserts edit block.label instrs_before;
    Multi_edit.add_replace edit block.label new_instr;
    Multi_edit.add_inserts edit block.label instrs_after;
    ()
  end
;;
