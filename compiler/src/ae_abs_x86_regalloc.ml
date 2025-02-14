open Std
open Ae_abs_x86_types
module Use_defs = Ae_abs_x86_use_defs
module Entity = Ae_entity_std
module Id = Entity.Id
module Name = Entity.Name

module Allocation = struct
  type t = Alloc_reg.t Vreg.Table.t [@@deriving sexp_of]
end

(* module wj]gable = Entity.Name.Table *)
module Regalloc = Ae_graph_greedy_regalloc
module Graph = Regalloc.Graph
module Mach_reg = Ae_x86_mach_reg
module Id_gen = Entity.Id_gen
module Call_conv = Ae_x86_call_conv

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
  let block = Func.get_start_block func in
  let live_out = Name.Table.create () in
  let graph = Graph.create () in
  let mach_reg_to_precolored_name = Hashtbl.create (module Mach_reg) in
  assert (not (Graph.mem graph (Name.create "next_id" func.next_id)));
  let next_id = Id_gen.of_id func.next_id in
  let find_precolored_name_or_add mach_reg =
    Hashtbl.find_or_add mach_reg_to_precolored_name mach_reg ~default:(fun () ->
      let precolored_name = Name.fresh next_id in
      Graph.add graph precolored_name;
      precolored_name)
  in
  Block.iter_instrs_backwards block ~f:(fun instr ->
    let defs = Use_defs.Instr.iter_defs instr |> Iter.to_list in
    (* make sure we at least add every use/def in, because the register allocator uses the domain of interference as all nodes *)
    List.iter defs ~f:(fun def ->
      Graph.add graph def;
      ());
    (* ensure that multiple defs interfere with each other *)
    iter_pairs defs ~f:(fun (def1, def2) -> Graph.add_edge graph def1 def2);
    let can_add_edge_to =
      let currently_defining =
        (*
           dst and src in movs are in the same equivalence class.
          We want these two to be allocated the same register,
          so add src as currently_defining, even though this is not actually the case.
        *)
        match instr with
        | Instr.Mov { src = Operand.Reg src; _ } -> src :: defs
        | _ -> defs
      in
      fun live -> not @@ List.mem ~equal:Vreg.equal currently_defining live
    in
    (* add interference edges *)
    Name.Table.iter_keys live_out
    |> Iter.filter ~f:can_add_edge_to
    |> Iter.iter ~f:(fun live ->
      List.iter defs |> Iter.iter ~f:(fun def -> Graph.add_edge graph def live);
      Use_defs.Instr.iter_mach_reg_defs instr ~f:(fun mach_reg ->
        let precolored_name = find_precolored_name_or_add mach_reg in
        Graph.add_edge graph precolored_name live;
        ());
      ());
    Ae_abs_x86_liveness.transfer live_out instr;
    (*
       for special instructions that take memory destination but also implicitly write registers such as RAX or RDX,
      we must prevent the dst operand from being allocated RAX or RDX or else it will be clobbered
    *)
    (match instr with
     | Instr.Bin { dst = Mem addr; op = Idiv | Imul | Imod; _ } ->
       let rax_name = find_precolored_name_or_add RAX in
       let rdx_name = find_precolored_name_or_add RDX in
       Ae_x86_address.iter_regs addr ~f:(fun vreg ->
         Graph.add_edge graph rdx_name vreg;
         Graph.add_edge graph rax_name vreg);
       ()
     | _ -> ());
    ());
  graph, mach_reg_to_precolored_name, Id_gen.next next_id
;;

(* TODO: add in callee saved later *)
let usable_mach_regs : Mach_reg.t list = Call_conv.caller_saved_without_r11

let alloc_func (func : Func.t) =
  let open Name.Table.Syntax in
  let module Color = Regalloc.Color in
  let graph, mach_reg_to_precolored_name, _next_id = build_graph func in
  let precolored =
    Hashtbl.to_alist mach_reg_to_precolored_name
    |> List.map ~f:(fun (_, vreg) -> vreg)
    |> Name.Set.of_list_exn
  in
  let coloring, max_color = Regalloc.color_graph graph precolored in
  let used_mach_regs = Hash_set.create (module Mach_reg) in
  let color_to_mach_reg : _ Regalloc.Color.Table.t = Id.Table.create () in
  let find_usable_mach_reg_and_use_up () =
    let res =
      List.find usable_mach_regs ~f:(fun usable_mach_reg ->
        not (Hash_set.mem used_mach_regs usable_mach_reg))
    in
    Option.iter res ~f:(fun reg ->
      Hash_set.add used_mach_regs reg;
      ());
    res
  in
  (* assign registers to the colors of precolored *)
  Name.Set.iter precolored ~f:(fun precolored ->
    let color = coloring.!(precolored) in
    let mach_reg =
      find_usable_mach_reg_and_use_up ()
      |> Option.value_or_thunk ~default:(fun () ->
        raise_s [%message "bug: Precolored register could not be allocated!"])
    in
    Id.Table.set color_to_mach_reg ~key:color ~data:(Alloc_reg.InReg mach_reg);
    ());
  assert (Id.to_int max_color >= -1);
  (* assign registers to colors *)
  Iter.Infix.(0 -- Id.to_int max_color)
  |> Iter.map ~f:(fun color -> (Id.unchecked_of_int color : Color.t))
  (* skip the precolored colors *)
  |> Iter.filter ~f:(fun color -> not @@ Id.Table.mem color_to_mach_reg color)
  |> Iter.iter ~f:(fun color ->
    let mach_reg = find_usable_mach_reg_and_use_up () in
    let allocation =
      Option.value_or_thunk mach_reg ~default:(fun () ->
        raise_s [%message "TODO: handle spilling into stack slots" [%here]])
      |> Alloc_reg.inreg
    in
    Id.Table.set color_to_mach_reg ~key:color ~data:allocation;
    ());
  (* finally, assign registers to vregs *)
  let allocation : _ Vreg.Table.t = Name.Table.create () in
  Graph.iter_vreg graph ~f:(fun vreg ->
    let color = coloring.!(vreg) in
    let alloc_reg = Id.Table.find_exn color_to_mach_reg color in
    allocation.!(vreg) <- alloc_reg;
    ());
  allocation
;;

(*
   TODO:
  insert spill instructions here procedure:
  let S be the set of spilled temporaries
  evict |S| registers to the stack
  create new temporaries representing the evicted registers,
  and make sure to add these to the allocation
  move the spilled temporaries to evicted temporaries.
  replace the temporaries with the evicted registers in the instruction.
  then reload the evicted temporaries
*)
