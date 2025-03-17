open Std
open Ae_generic_ir_import

module Make (Ir : Ir) = struct
  open Ir.Std

  open struct
    let is_on_top table ~equal ~key ~data =
      Ident.Table.find_multi table key |> List.hd |> Option.equal equal (Some data)
    ;;

    let add_multi_set table ~key ~data =
      Ident.Table.update table key ~f:(function
        | None -> Ident.Set.singleton data
        | Some s -> Ident.Set.add s data)
    ;;
  end

  module Live_set = struct
    type t = Temp.Set.t Label.Table.t [@@deriving sexp_of]

    let find t label = Ident.Table.find t label |> Option.value ~default:Ident.Set.empty
  end

  let compute_defs_and_upward_exposed func =
    let module Table = Ident.Table in
    let open Table.Syntax in
    let defs : Label.t list Temp.Table.t = Table.create () in
    let upward_exposed = Table.create () in
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
          Table.add_multi upward_exposed ~key:use ~data:block.label
        end
      end;
      begin
        let@: def = Instr.iter_defs instr.i in
        if no_definition_yet def
        then begin
          Table.add_multi defs ~key:def ~data:block.label
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
    let module Table = Ident.Table in
    let open Table.Syntax in
    let defs = Table.create () in
    let uses = Table.create () in
    begin
      let@: block = Func.iter_blocks func in
      let@: instr = Block.iter_fwd block in
      begin
        let@: def = Instr.iter_defs instr.i in
        defs.!(def) <- block.label
      end;
      begin
        let@: use = Instr.iter_uses instr.i in
        if not (is_on_top uses ~equal:Label.equal ~key:use ~data:block.label)
        then begin
          Table.add_multi uses ~key:use ~data:block.label
        end
      end
    end;
    defs, uses
  ;;

  let compute_non_ssa ~(pred_table : Adj_table.t) func =
    let module Table = Ident.Table in
    let open Table.Syntax in
    let block_mark : _ Label.Table.t = Table.create () in
    let defs_table, upward_exposed_table = compute_defs_and_upward_exposed func in
    let live_in : _ Label.Table.t = Table.create () in
    let live_in_top = Table.create () in
    let live_out : _ Label.Table.t = Table.create () in
    let live_out_top = Table.create () in
    let rec up_and_mark (label : Label.t) (temp : Temp.t) =
      if
        let not_added_in_live_out_yet =
          Table.find live_out_top label |> [%equal: Temp.t option] (Some temp) |> not
        in
        not_added_in_live_out_yet
      then begin
        add_multi_set live_out ~key:label ~data:temp;
        live_out_top.!(label) <- temp
      end;
      if
        let killed_in_block =
          Table.find block_mark label |> [%equal: Temp.t option] (Some temp)
        in
        killed_in_block
      then ()
      else if
        let already_propagated =
          Table.find live_in_top label |> [%equal: Temp.t option] (Some temp)
        in
        already_propagated
      then ()
      else begin
        add_multi_set live_in ~key:label ~data:temp;
        live_in_top.!(label) <- temp;
        let@: pred = List.iter pred_table.!(label) in
        up_and_mark pred temp
      end
    in
    begin
      let@: temp, def_in_labels = Table.iteri defs_table in
      let upward_exposed = Table.find_multi upward_exposed_table temp in
      begin
        let@: def_in_label = List.iter def_in_labels in
        block_mark.!(def_in_label) <- temp
      end;
      begin
        let@: label = List.iter upward_exposed in
        if
          let not_propagated_yet =
            Table.find live_in_top label |> [%equal: Temp.t option] (Some temp) |> not
          in
          not_propagated_yet
        then begin
          add_multi_set live_in ~key:label ~data:temp;
          live_in_top.!(label) <- temp;
          begin
            let@: pred = List.iter pred_table.!(label) in
            up_and_mark pred temp
          end
        end
      end
    end;
    live_in, live_out
  ;;

  let compute ~pred_table func =
    let module Table = Ident.Table in
    let open Table.Syntax in
    let open Ae_trace in
    let def_to_label, use_to_labels = compute_def_use_labels func in
    let live_in = Table.create () in
    let live_in_top = Table.create () in
    let live_out = Table.create () in
    let live_out_top = Table.create () in
    let rec up_and_mark label temp =
      if Label.equal label def_to_label.!(temp)
      then ()
      else if Table.find live_in_top label |> [%equal: Temp.t option] (Some temp)
      then ()
      else begin
        add_multi_set live_in ~key:label ~data:temp;
        live_in_top.!(label) <- temp;
        begin
          let@: pred = List.iter pred_table.!(label) in
          if Table.find live_out_top pred |> [%equal: Temp.t option] (Some temp) |> not
          then begin
            add_multi_set live_out ~key:pred ~data:temp;
            live_out_top.!(pred) <- temp
          end;
          up_and_mark pred temp
        end
      end
    in
    begin
      let@: temp = Table.iter_keys def_to_label in
      begin
        let@: use_in_label = List.iter @@ Table.find_multi use_to_labels temp in
        up_and_mark use_in_label temp
      end
    end;
    live_in, live_out
  ;;

  let backwards_transfer instr live =
    let live = Instr.iter_defs instr |> Iter.fold ~init:live ~f:Ident.Set.remove in
    let live = Instr.iter_uses instr |> Iter.fold ~init:live ~f:Ident.Set.add in
    live
  ;;
end
