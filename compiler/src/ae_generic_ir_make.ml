open Std
open Ae_generic_ir_sigs
module Graph = Ae_data_graph_std

open struct
  module Entity = Ae_entity_std
  module Ident = Entity.Ident
  module Label_entity = Ae_label_entity
  module Label = Label_entity.Ident
  module Entity_graph_utils = Ae_entity_graph_utils
  module Dominators = Ae_dominators
  module Stack_slot_entity = Ae_stack_slot_entity
  module Stack_slot = Stack_slot_entity.Ident
  module Id_gen = Entity.Id_gen
end

module Make_ir (Arg : Arg) = struct
  module Arg = Arg

  (* very important! *)
  (* the open Arg must be after the module alias *)
  open Arg
  module Temp = Temp_entity.Ident

  module Instr_ext = struct
    let iter_labels i = Instr.iter_block_calls i |> Iter.map ~f:Block_call.label
    let labels_list i = iter_labels i |> Iter.to_list
  end

  module Instr' = struct
    module T = struct
      type t =
        { i : Instr.t
        ; index : int
        ; info : Info.t option
        }
      [@@deriving sexp_of]

      let to_int t = t.index
    end

    include T

    let instr t = t.i
    let map t ~f = { t with i = f t.i }
    let create ?info i index = { i; index; info }
    let create_unindexed ?info i = { i; index = -1; info }
    let invalid_nop = { i = Instr.nop; index = -1; info = None }

    module Table = Entity.Table.Make (T)
    module Map = Entity.Map.Make (T)
  end

  module Block = struct
    module T = struct
      type t =
        { label : Label.t
        ; body : Instr'.t iarray
        }
      [@@deriving sexp_of]

      let to_int t = Entity.Id.to_int t.label.id
    end

    include T

    let iter_fwd t ~f = Arrayp.iter t.body ~f
    let iter_bwd t ~f = Arrayp.iteri_rev t.body ~f:(fun _ x -> f x)

    let create label body =
      { label
      ; body = Arrayp.mapi body ~f:(fun index instr -> { instr with Instr'.index })
      }
    ;;

    let create_id label body = { label; body }
    let instrs t = t.body

    let find_control t =
      Arrayp.find t.body ~f:(fun x -> Instr.is_control x.i)
      |> Option.value_exn
           ~error:(Error.create "Could not find jump instruction in block" t sexp_of_t)
    ;;

    let find_block_params t =
      Arrayp.find t.body ~f:(fun instr -> Instr.is_block_params instr.i)
      |> Option.value_exn
           ~error:
             (Error.create "Could not find block params instruction in block" t sexp_of_t)
    ;;

    module Table = Entity.Table.Make (T)
    module Map = Entity.Map.Make (T)
  end

  module Adj_map = struct
    type t = Label.t list Label.Map.t
  end

  module Adj_table = struct
    type t = Label.t list Label.Table.t
  end

  module Edit = struct
    type t =
      | Insert of Instr'.t
      | Remove of Instr'.t
      | Replace of Instr'.t
    [@@deriving sexp_of]

    let insert i = Insert i
    let remove i = Remove i
    let replace i = Replace i

    let apply ?no_sort edit (block : Block.t) =
      let inserts, removes, replaces =
        List.partition3_map
          ~f:(function
            | Insert x -> `Fst x
            | Remove x -> `Snd x
            | Replace x -> `Trd x)
          edit
      in
      let inserts = List.map ~f:(fun i -> i.index, i) inserts in
      let instrs = Arrayp.to_array block.body in
      List.iter removes ~f:(fun i -> Array.set instrs i.index Instr'.invalid_nop);
      List.iter replaces ~f:(fun i -> Array.set instrs i.index i);
      let instrs =
        Ae_array_utils.apply_inserts ?no_sort Instr'.invalid_nop inserts instrs
      in
      let instrs =
        if List.is_empty removes
        then instrs
        else Array.filter instrs ~f:(fun instr -> not (Instr.is_nop instr.i))
      in
      if (not (List.is_empty removes)) || not (List.is_empty inserts)
      then begin
        let index = ref 0 in
        Array.map_inplace instrs ~f:(fun instr ->
          let instr = { instr with index = !index } in
          incr index;
          instr)
      end;
      let instrs = Arrayp.of_array instrs in
      { block with body = instrs }
    ;;
  end

  module Multi_edit = struct
    type t = Edit.t list Label.Table.t [@@deriving sexp_of]

    let create () = Ident.Table.create ()

    let add_insert t label instr =
      Ident.Table.add_multi t ~key:label ~data:(Edit.insert instr);
      ()
    ;;

    let add_edits t label edits =
      Ident.Table.update t label ~f:(function
        | None -> List.rev edits
        | Some es -> List.rev_append edits es)
    ;;

    let add_inserts t label inserts = add_edits t label @@ List.map inserts ~f:Edit.insert

    let add_remove t label instr =
      Ident.Table.add_multi t ~key:label ~data:(Edit.remove instr);
      ()
    ;;

    let add_replace t label instr =
      Ident.Table.add_multi t ~key:label ~data:(Edit.replace instr);
      ()
    ;;

    let apply_blocks ?no_sort t blocks =
      Ident.Table.iteri t
      |> Iter.fold ~init:blocks ~f:(fun blocks (label, edits) ->
        let edits = List.rev edits in
        let block =
          Ident.Map.find blocks label
          |> Option.value_or_thunk ~default:(fun () ->
            Block.create label (Arrayp.of_list []))
        in
        let block = Edit.apply ?no_sort edits block in
        Ident.Map.set blocks ~key:label ~data:block)
    ;;
  end

  module Stack_builder = struct
    type t =
      { mutable stack_slot_gen : Stack_slot_entity.Id.t
      ; mutable stack_slots : (Stack_slot.t * Ty.t) list
      }
  end

  module Func = struct
    type t =
      { name : string
      ; blocks : Block.t Label.Map.t
      ; start : Label.t
      ; next_temp_id : Temp_entity.Id.t
      ; next_label_id : Label_entity.Id.t
      ; data : Func_data.t
      }
    [@@deriving sexp_of]

    let start_block func =
      Entity.Ident.Map.find func.blocks func.start
      |> Option.value_or_thunk ~default:(fun () ->
        raise_s
          [%message "invariant broken: start block did not exist" (func.start : Label.t)])
    ;;

    let find_block_exn func label = Entity.Ident.Map.find_exn func.blocks label

    let succ_map func =
      func.blocks
      |> Entity.Ident.Map.map ~f:(fun b ->
        let i = Block.find_control b in
        Instr_ext.labels_list i.i)
    ;;

    let succ_list func =
      func.blocks
      |> Entity.Ident.Map.to_alist
      |> List.map ~f:(fun (lab, b) ->
        let i = Block.find_control b in
        lab, Instr_ext.labels_list i.i)
    ;;

    let succ_table func = succ_list func |> Entity.Ident.Table.of_list

    let pred_table_of_succ succ =
      Entity_graph_utils.(get_pred_table (graph_of_adj_table succ))
    ;;

    let pred_table func = pred_table_of_succ (succ_table func)
    let graph func = Entity_graph_utils.graph_of_adj_table (succ_table func)
    let bi_graph func = graph func |> Entity_graph_utils.to_bi

    let compute_idoms ?graph t =
      Dominators.compute
        ~start:t.start
        (Option.value_or_thunk graph ~default:(fun () -> bi_graph t))
    ;;

    let compute_dom_tree ?graph t =
      let idoms = compute_idoms ?graph t in
      Dominators.Tree.of_immediate idoms
    ;;

    let iter_blocks t = t.blocks |> Ident.Map.iter

    let get_ty_table func =
      let module Table = Ident.Table in
      let open Table.Syntax in
      let table = Table.create () in
      begin
        let@: block = iter_blocks func in
        let@: instr = Block.iter_fwd block in
        let@: def, ty = Instr.iter_defs_with_ty instr.i in
        if not (Table.mem table def)
        then begin
          table.!(def) <- ty
        end
        else begin
          assert (Ty.equal table.!(def) ty)
        end
      end;
      table
    ;;

    let labels_reverse_postorder func =
      Label_entity.Dfs.reverse_postorder ~start:[ func.start ] (graph func)
    ;;

    let labels_postorder func =
      Label_entity.Dfs.postorder ~start:[ func.start ] (graph func)
    ;;

    let apply_multi_edit ?no_sort edit func =
       { func with blocks = Multi_edit.apply_blocks ?no_sort edit func.blocks }
    ;;

    let apply_temp_gen temp_gen func = { func with next_temp_id = Id_gen.next temp_gen }
  end

  module Program = struct
    type t = { funcs : Func.t } [@@deriving sexp_of]
  end
end
