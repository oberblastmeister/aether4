open Std
open Ae_generic_ir_import

module Make (Ir : Ir) = struct
  open Make_std(Ir)

  let check_all_temps_unique func =
    let module Table = Ident.Table in
    let open Table.Syntax in
    let errors = Stack.create () in
    let defines = Table.create () in
    begin
      let@: block = Func.iter_blocks func in
      let@: instr = Block.iter_fwd block in
      begin
        let@: def = Instr.iter_defs instr.i in
        begin
          if Table.mem defines def
          then begin
            Stack.push
              errors
              (Error.create "Temp was defined more than once" def Temp.sexp_of_t)
          end
          else begin
            defines.!(def) <- ()
          end
        end
      end
    end;
    if Stack.is_empty errors
    then Ok ()
    else Stack.to_list errors |> Error.of_list |> Error
  ;;

  let check_dominators (func : Func.t) =
    let dom_tree = Func.compute_dom_tree func in
    let start = func.start in
    let errors = Stack.create () in
    let rec go scope label =
      let block = Ident.Map.find_exn func.blocks label in
      let scope = ref scope in
      begin
        let@: instr = Block.iter_fwd block in
        begin
          let@: use = Instr.iter_uses instr.i in
          if not (Ident.Set.mem !scope use)
          then begin
            Stack.push errors
            @@ Error.create "A use was not dominated by a define" use Temp.sexp_of_t
          end
        end;
        begin
          let@: def = Instr.iter_defs instr.i in
          scope := Ident.Set.add !scope def
        end
      end;
      begin
        let@: next_label = List.iter @@ Dominators.Tree.children dom_tree label in
        go !scope next_label
      end
    in
    go Ident.Set.empty start;
    if Stack.is_empty errors
    then Ok ()
    else Stack.to_list errors |> Error.of_list |> Error
  ;;

  let check func =
    let open Or_error.Let_syntax in
    let%bind () = check_all_temps_unique func in
    let%bind () = check_dominators func in
    Ok ()
  ;;
end
