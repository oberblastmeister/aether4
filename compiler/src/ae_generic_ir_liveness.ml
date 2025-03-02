open Std
open Ae_generic_ir_import

module Make (Ir : Ir) = struct
  open Ir.Std

  open struct
    let is_on_top table ~equal ~key ~data =
      Ident.Table.find_multi table key |> List.hd |> Option.equal equal (Some data)
    ;;
  end

  module Live_list = struct
    type t = Temp.t list Label.Table.t [@@deriving sexp_of]
  end

  module Live_set = struct
    type t = Temp.Set.t Label.Table.t [@@deriving sexp_of]
  end

  let compute_defs_and_upward_exposed_maybe ?(compute_upward_exposed : unit option) func =
    let module Table = Ident.Table in
    let open Table.Syntax in
    let defs : Label.t list Temp.Table.t = Table.create () in
    let defs_to_ty : Ty.t Temp.Table.t = Table.create () in
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
      if Option.is_some compute_upward_exposed
      then begin
        let@: use = Instr.iter_uses instr.i in
        if no_definition_yet use && didn't_add_upward_exposed_yet use
        then begin
          Table.add_multi upward_exposed ~key:use ~data:block.label
        end
      end;
      begin
        let@: def, ty = Instr.iter_defs_with_ty instr.i in
        if not (Table.mem defs_to_ty def)
        then begin
          defs_to_ty.!(def) <- ty
        end
        else begin
          assert (Ty.equal defs_to_ty.!(def) ty)
        end;
        if no_definition_yet def
        then begin
          Table.add_multi defs ~key:def ~data:block.label
        end
      end
    end;
    defs, defs_to_ty, upward_exposed
  ;;

  let compute_defs_and_upward_exposed func =
    compute_defs_and_upward_exposed_maybe ~compute_upward_exposed:() func
  ;;

  let compute_def_blocks func =
    let defs, def_to_ty, _ = compute_defs_and_upward_exposed_maybe func in
    defs, def_to_ty
  ;;

  let compute_non_ssa_live_list ~(pred_table : Adj_table.t) func =
    let module Table = Ident.Table in
    let open Table.Syntax in
    let block_mark : _ Label.Table.t = Table.create () in
    let defs_table, _, upward_exposed_table = compute_defs_and_upward_exposed func in
    let live_in : _ Label.Table.t = Table.create () in
    let live_out : _ Label.Table.t = Table.create () in
    let rec up_and_mark (label : Label.t) (temp : Temp.t) =
      if
        let not_added_in_live_out_yet =
          not (is_on_top live_out ~equal:Temp.equal ~key:label ~data:temp)
        in
        not_added_in_live_out_yet
      then begin
        Table.add_multi live_out ~key:label ~data:temp
      end;
      if
        let killed_in_block =
          Table.find block_mark label |> [%equal: Temp.t option] (Some temp)
        in
        killed_in_block
      then ()
      else if
        let already_propagated =
          is_on_top live_in ~equal:Temp.equal ~key:label ~data:temp
        in
        already_propagated
      then ()
      else begin
        Table.add_multi live_in ~key:label ~data:temp;
        let@: pred = List.iter pred_table.!(label) in
        up_and_mark pred temp
      end
    in
    begin
      let@: temp, defs = Table.iteri defs_table in
      let upward_exposed = Table.find_multi upward_exposed_table temp in
      begin
        let@: def_in = List.iter defs in
        block_mark.!(def_in) <- temp
      end;
      begin
        let@: label = List.iter upward_exposed in
        if
          let not_propagated_yet =
            not (is_on_top live_in ~equal:Temp.equal ~key:label ~data:temp)
          in
          not_propagated_yet
        then begin
          Table.add_multi live_in ~key:label ~data:temp;
          begin
            let@: pred = List.iter pred_table.!(label) in
            up_and_mark pred temp
          end
        end
      end
    end;
    live_in, live_out
  ;;

  let compute_non_ssa ~pred_table func =
    let live_in, live_out = compute_non_ssa_live_list ~pred_table func in
    ( Ident.Table.map ~f:Ident.Set.of_list_exn live_in
    , Ident.Table.map ~f:Ident.Set.of_list_exn live_out )
  ;;
end
