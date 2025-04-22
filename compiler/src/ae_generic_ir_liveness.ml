open Std
open Ae_generic_ir_import

module Make (Ir : Ir) = struct
  open Ir

  open struct
    let is_on_top table ~equal ~key ~data =
      Temp.Table.find_multi table key |> List.hd |> Option.equal equal (Some data)
    ;;

    let add_multi_set table ~key ~data =
      Label.Table.update table key ~f:(function
        | None -> Temp.Set.singleton data
        | Some s -> Set.add s data)
    ;;
  end

  module Live_set = struct
    type t = Temp.Set.t Label.Table.t [@@deriving sexp_of]

    (*
       we need to protect against None, because the block may not have been traversed.
      If an entire block has dead code, then it will never be traversed because its variables were never used anywhere.
    *)
    let find t label = Label.Table.find t label |> Option.value ~default:Temp.Set.empty
  end

  module Next_use_table = struct
    type t = int Temp.Map.t Label.Table.t [@@deriving sexp_of]

    let find t label = Label.Table.find t label |> Option.value ~default:Temp.Map.empty
  end

  let compute_defs_and_upward_exposed func =
    let defs : Label.t list Temp.Table.t = Temp.Table.create () in
    let upward_exposed = Temp.Table.create () in
    begin
      let@: block = Func.iter_blocks func in
      let@: instr = Block.iter_fwd block in
      let no_definition_yet temp =
        not (is_on_top defs ~equal:Label.equal ~key:temp ~data:block.label)
      in
      let didn't_add_upward_exposed_yet temp =
        not (is_on_top upward_exposed ~equal:Label.equal ~key:temp ~data:block.label)
      in
      begin
        let@: use = Instr.iter_uses instr.i in
        if no_definition_yet use && didn't_add_upward_exposed_yet use
        then begin
          Temp.Table.add_multi upward_exposed ~key:use ~data:block.label
        end
      end;
      begin
        let@: def = Instr.iter_defs instr.i in
        if no_definition_yet def
        then begin
          Temp.Table.add_multi defs ~key:def ~data:block.label
        end
      end
    end;
    defs, upward_exposed
  ;;

  let compute_def_blocks_non_ssa func =
    let defs, _ = compute_defs_and_upward_exposed func in
    defs
  ;;

  let compute_def_use_labels func =
    let defs = Temp.Table.create () in
    let uses = Temp.Table.create () in
    begin
      let@: block = Func.iter_blocks func in
      let@: instr = Block.iter_fwd block in
      begin
        let@: def = Instr.iter_defs instr.i in
        defs.Temp.Table.Syntax.!(def) <- block.label
      end;
      begin
        let@: use = Instr.iter_uses instr.i in
        if
          Temp.Table.find_multi uses use
          |> List.hd
          |> Option.map ~f:(Fn.compose __.Block.label fst)
          |> Option.equal Label.equal (Some block.label)
          |> not
        then begin
          Temp.Table.add_multi uses ~key:use ~data:(block, instr)
        end
      end
    end;
    defs, uses
  ;;

  let compute_non_ssa ~(pred_table : Adj_table.t) func =
    let block_mark : _ Label.Table.t = Label.Table.create () in
    let defs_table, upward_exposed_table = compute_defs_and_upward_exposed func in
    let live_in : _ Label.Table.t = Label.Table.create () in
    let live_in_top = Label.Table.create () in
    let live_out : _ Label.Table.t = Label.Table.create () in
    let live_out_top = Label.Table.create () in
    let rec up_and_mark (label : Label.t) (temp : Temp.t) =
      if
        let added_in_live_out =
          Label.Table.find live_out_top label |> [%equal: Temp.t option] (Some temp)
        in
        added_in_live_out
      then ()
      else begin
        add_multi_set live_out ~key:label ~data:temp;
        live_out_top.Label.Table.Syntax.!(label) <- temp
      end;
      if
        let killed_in_block =
          Label.Table.find block_mark label |> [%equal: Temp.t option] (Some temp)
        in
        killed_in_block
      then ()
      else if
        let already_propagated =
          Label.Table.find live_in_top label |> [%equal: Temp.t option] (Some temp)
        in
        already_propagated
      then ()
      else begin
        add_multi_set live_in ~key:label ~data:temp;
        live_in_top.Label.Table.Syntax.!(label) <- temp;
        let@: pred = List.iter pred_table.Label.Table.Syntax.!(label) in
        up_and_mark pred temp
      end
    in
    begin
      let@: temp, def_in_labels = Temp.Table.iteri defs_table in
      let upward_exposed = Temp.Table.find_multi upward_exposed_table temp in
      begin
        let@: def_in_label = List.iter def_in_labels in
        block_mark.Label.Table.Syntax.!(def_in_label) <- temp
      end;
      begin
        let@: label = List.iter upward_exposed in
        if
          let not_propagated_yet =
            Label.Table.find live_in_top label
            |> [%equal: Temp.t option] (Some temp)
            |> not
          in
          not_propagated_yet
        then begin
          add_multi_set live_in ~key:label ~data:temp;
          live_in_top.Label.Table.Syntax.!(label) <- temp;
          begin
            let@: pred = List.iter pred_table.Label.Table.Syntax.!(label) in
            up_and_mark pred temp
          end
        end
      end
    end;
    live_in, live_out
  ;;

  let compute ~pred_table func =
    let open Ae_trace in
    let def_to_label, use_to_labels = compute_def_use_labels func in
    let live_in = Label.Table.create () in
    let live_in_top = Label.Table.create () in
    let live_out = Label.Table.create () in
    let live_out_top = Label.Table.create () in
    let rec up_and_mark label temp =
      if Label.equal label def_to_label.Temp.Table.Syntax.!(temp)
      then ()
      else if Label.Table.find live_in_top label |> [%equal: Temp.t option] (Some temp)
      then ()
      else begin
        add_multi_set live_in ~key:label ~data:temp;
        live_in_top.Label.Table.Syntax.!(label) <- temp;
        begin
          let@: pred = List.iter pred_table.Label.Table.Syntax.!(label) in
          if
            Label.Table.find live_out_top pred
            |> [%equal: Temp.t option] (Some temp)
            |> not
          then begin
            add_multi_set live_out ~key:pred ~data:temp;
            live_out_top.Label.Table.Syntax.!(pred) <- temp;
            up_and_mark pred temp
          end
        end
      end
    in
    begin
      let@: temp = Temp.Table.iter_keys def_to_label in
      begin
        let@: use_block, _ = List.iter @@ Temp.Table.find_multi use_to_labels temp in
        up_and_mark use_block.label temp
      end
    end;
    live_in, live_out
  ;;

  (* very similar implementation as in liveness *)
  (* TODO: share code with liveness implementation *)
  let compute_next_use_distance ~pred_table func =
    (* lattice operations *)
    let merge_info table ~label ~(temp : Temp.t) ~next_use_distance =
      Label.Table.update table label ~f:(function
        | None -> Temp.Map.singleton temp next_use_distance
        | Some m ->
          Map.update
            m
            temp
            ~f:
              (Option.value_map ~default:next_use_distance ~f:(fun next_use_distance' ->
                 assert (next_use_distance < next_use_distance');
                 next_use_distance)))
    in
    let did_change table ~label ~temp ~next_use_distance =
      Label.Table.find table label
      |> Option.map ~f:(fun (temp', next_use_distance') ->
        (not (Temp.equal temp temp')) || next_use_distance < next_use_distance')
      |> Option.value ~default:true
    in
    let def_to_label, use_to_labels = compute_def_use_labels func in
    let live_in = Label.Table.create () in
    let live_in_top = Label.Table.create () in
    let live_out = Label.Table.create () in
    let live_out_top = Label.Table.create () in
    let rec up_and_mark label_to_first_use (block : Block.t) (temp : Temp.t) =
      let next_use_distance =
        Map.find label_to_first_use block.label
        |> Option.map ~f:__.Instr'.index
        |> Option.value_or_thunk ~default:(fun () ->
          Arrayp.length block.Block.body
          + Map.find_exn live_out.Label.Table.Syntax.!(block.label) temp)
      in
      if Label.equal block.label def_to_label.Temp.Table.Syntax.!(temp)
      then ()
      else if not (did_change live_in_top ~label:block.label ~temp ~next_use_distance)
      then ()
      else begin
        merge_info live_in ~label:block.label ~temp ~next_use_distance;
        live_in_top.Label.Table.Syntax.!(block.label) <- temp, next_use_distance;
        begin
          let@: pred = List.iter pred_table.Label.Table.Syntax.!(block.label) in
          if did_change live_out_top ~label:pred ~temp ~next_use_distance
          then begin
            merge_info live_out ~label:pred ~temp ~next_use_distance;
            live_out_top.Label.Table.Syntax.!(pred) <- temp, next_use_distance;
            up_and_mark label_to_first_use (Func.find_block_exn func pred) temp
          end
        end
      end
    in
    begin
      let@: temp = Temp.Table.iter_keys def_to_label in
      let uses_labels = Temp.Table.find_multi use_to_labels temp in
      let label_to_first_use =
        List.map uses_labels ~f:(fun (block, instr) -> block.label, instr)
        |> Label.Map.of_alist_exn
      in
      begin
        let@: used_in_block, _ = List.iter @@ uses_labels in
        up_and_mark label_to_first_use used_in_block temp
      end
    end;
    live_in, live_out
  ;;

  let next_use_backwards_transfer instr next_use =
    let next_use = Instr.iter_defs instr |> Iter.fold ~init:next_use ~f:Map.remove in
    let next_use = Map.map next_use ~f:(( + ) 1) in
    let uses = Instr.iter_uses instr |> Iter.to_list in
    let next_use =
      List.fold uses ~init:next_use ~f:(fun next_use use ->
        Map.update next_use use ~f:(Option.value ~default:0))
    in
    next_use
  ;;

  let backwards_transfer instr live =
    let live = Instr.iter_defs instr |> Iter.fold ~init:live ~f:Set.remove in
    let live = Instr.iter_uses instr |> Iter.fold ~init:live ~f:Set.add in
    live
  ;;

  let compute_deaths ~live_out (block : Block.t) =
    let live_out = ref live_out in
    let deaths =
      begin
        let@: instr = Arrayp.map_rev block.body in
        let deaths =
          Instr.iter_uses instr.i
          |> Iter.filter ~f:(fun use ->
            (* this is the last use *)
            not (Set.mem !live_out use))
          |> Iter.to_list
          |> Temp.Set.of_list
        in
        live_out := backwards_transfer instr.i !live_out;
        deaths
      end
    in
    deaths
  ;;
end
