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
module Destruct_ssa = Ae_abs_x86_destruct_ssa
open Ae_trace

let mach_reg_id off mach_reg = Id.offset off (Mach_reg.to_enum mach_reg)

let mach_reg_ident ?info off mach_reg =
  let id = mach_reg_id off mach_reg in
  Ident.create ?info (Mach_reg.to_string mach_reg) id
;;

let spill_regular_instr
      ~spilled_temp_to_slot
      ~spilled_colors
      ~get_temp_color
      ~get_evicted_temp_and_slot_for_mach_reg
      ~(instr : Instr'.t)
  =
  let module Table = Ident.Table in
  let open Table.Syntax in
  let evicted_temps = ref [] in
  let used_non_spilled_colors =
    Instr.iter_defs instr.i
    |> Iter.append (Instr.iter_uses instr.i)
    |> Iter.map ~f:(fun temp -> get_temp_color temp)
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
       & Traverse.filtered ~f:(fun temp -> Set.mem spilled_colors (get_temp_color temp)))
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
       & Traverse.filtered ~f:(fun temp -> Set.mem spilled_colors (get_temp_color temp)))
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

let spill_ssa ~spilled_temp_to_slot ~spilled_colors ~get_temp_color (instr' : Instr'.t) =
  assert (Instr.is_block_params instr'.i || Instr.is_control instr'.i);
  let@: instr = Instr'.map instr' in
  match Instr.block_params_val instr with
  | Some block_params ->
    let block_params =
      begin
        let@: location =
          (List.map & Traverse.of_field Block_param.Fields.param) block_params
        in
        match location with
        | Temp temp when Set.mem spilled_colors (get_temp_color temp) ->
          let slot = spilled_temp_to_slot temp in
          Slot slot
        | _ -> location
      end
    in
    Instr.block_params block_params
  | None ->
    let@: block_call = Instr.map_block_calls instr in
    let@: location = (Traverse.of_field Block_call.Fields.args & List.map) block_call in
    (match location with
     | Temp temp when Set.mem spilled_colors (get_temp_color temp) ->
       let slot = spilled_temp_to_slot temp in
       Slot slot
     | _ -> location)
;;

(*
   TODO: call this when not totally in ssa form yet by spilling the locations
  
   invariant: 

   this must be only called after ssa destruction.
   so the resulting func is not in ssa form.
   this will insert instructions that redefine temporaries.
   
   every thing that is not spilled must be present in allocation.
*)
let spill_func ~mach_reg_id ~get_temp_color ~spilled_colors (func : Func.t) =
  let module Table = Ident.Table in
  let open Table.Syntax in
  let stack_builder = Func.create_stack_builder func in
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
    fun temp -> Int_table.find_exn spilled_color_to_slot (get_temp_color temp)
  in
  let mach_reg_to_evicted_temp = Hashtbl.create (module Int) in
  let edit = Multi_edit.create () in
  begin
    let@: block = Func.iter_blocks func in
    let@: instr = Block.iter_fwd block in
    if Instr.is_block_params instr.i || Instr.is_control instr.i
    then begin
      let instr = spill_ssa ~spilled_temp_to_slot ~spilled_colors ~get_temp_color instr in
      Multi_edit.add_replace edit block.label instr
    end
    else begin
      let open Ident.Table.Syntax in
      let instrs_before, new_instr, instrs_after =
        let get_evicted_temp_and_slot_for_mach_reg mach_reg =
          Hashtbl.find_or_add mach_reg_to_evicted_temp mach_reg ~default:(fun () ->
            let stack_slot =
              Stack_builder.alloc ~name:"evicted_slot" stack_builder Qword
            in
            let temp = mach_reg_ident mach_reg_id (Mach_reg.of_enum_exn mach_reg) in
            temp, stack_slot)
        in
        spill_regular_instr
          ~spilled_temp_to_slot
          ~spilled_colors
          ~get_temp_color
          ~get_evicted_temp_and_slot_for_mach_reg
          ~instr
      in
      Multi_edit.add_inserts edit block.label instrs_before;
      Multi_edit.add_replace edit block.label new_instr;
      Multi_edit.add_inserts edit block.label instrs_after;
      ()
    end
  end;
  Func.apply_multi_edit edit func |> Func.apply_stack_builder stack_builder
;;
