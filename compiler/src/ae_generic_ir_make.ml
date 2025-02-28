open Std
open Ae_generic_ir_sigs

open struct
  module Entity = Ae_entity_std
  module Ident = Entity.Ident
  module Label_entity = Ae_label_entity
  module Label = Label_entity.Ident
  module Entity_graph_utils = Ae_entity_graph_utils
  module Dominators = Ae_dominators
end

module Make_ir (Arg : Arg) = struct
  module Arg = Arg
  open Arg
  module Temp = Temp_entity.Ident

  module Instr_ext = struct
    let jumps_labels i = Instr.jumps i |> (Option.map & List.map) ~f:(fun b -> b.label)
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

    let map t ~f = { t with i = f t.i }
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

    let find_jump t =
      let len = Arrayp.length t.body in
      Arrayp.find_mapi t.body ~f:(fun i _ ->
        let i = len - i - 1 in
        let x = t.body.@(i) in
        Option.map (Instr.jumps x.i) ~f:(Fn.const x))
      |> Option.value_or_thunk ~default:(fun () ->
        raise_s [%message "Could not find jump instruction in block" (t : t)])
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

  module Func = struct
    type t =
      { name : string
      ; blocks : Block.t Label.Map.t
      ; start : Label.t
      ; next_temp_id : Temp_entity.Id.t
      ; next_label_id : Label_entity.Id.t
      }
    [@@deriving sexp_of]

    let start_block func =
      Entity.Ident.Map.find func.blocks func.start
      |> Option.value_or_thunk ~default:(fun () ->
        raise_s
          [%message "invariant broken: start block did not exist" (func.start : Label.t)])
    ;;

    let succ_map func =
      func.blocks
      |> Entity.Ident.Map.map ~f:(fun b ->
        let i = Block.find_jump b in
        Instr_ext.jumps_labels i.i
        |> Option.value_exn
             ~error:(Error.create "should be a jump instruction" i Instr'.sexp_of_t))
    ;;

    let succ_list func =
      func.blocks
      |> Entity.Ident.Map.to_alist
      |> List.map ~f:(fun (lab, b) ->
        let i = Block.find_jump b in
        ( lab
        , Instr_ext.jumps_labels i.i
          |> Option.value_exn
               ~error:(Error.create "should be a jump instruction" i Instr'.sexp_of_t) ))
    ;;

    let succ_table func = succ_list func |> Entity.Ident.Table.of_list

    let pred_table_of_succ succ =
      Entity_graph_utils.(get_pred_table (graph_of_adj_table succ))
    ;;

    let pred_table func = pred_table_of_succ (succ_table func)
    let graph func = Entity_graph_utils.graph_of_adj_map (succ_map func)
    let bi_graph func = graph func |> Entity_graph_utils.to_bi

    let compute_idoms ?graph t =
      Dominators.Immediate.compute
        ~start:t.start
        (Option.value_or_thunk graph ~default:(fun () -> bi_graph t))
    ;;

    let iter_blocks t = t.blocks |> Ident.Map.iter
  end

  module Edit = struct
    type t =
      | Insert of Instr'.t
      | Remove of Instr'.t
    [@@deriving sexp_of]

    let insert i = Insert i
    let remove i = Remove i

    let apply ?no_sort edit (block : Block.t) =
      let inserts, removes =
        List.partition_map
          ~f:(function
            | Insert x -> First x
            | Remove x -> Second x)
          edit
      in
      let inserts = List.map ~f:(fun i -> i.index, i) inserts in
      let body = Arrayp.to_array block.body in
      List.iter removes ~f:(fun i -> Array.set body i.index Instr'.invalid_nop);
      let body = Ae_array_utils.apply_inserts ?no_sort Instr'.invalid_nop inserts body in
      let res =
        Array.filter_mapi body ~f:(fun index instr ->
          if Instr.is_nop instr.i then None else Some { instr with index })
      in
      let res = Arrayp.of_array res in
      { block with body = res }
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
      Ident.Table.update t label ~f:(Option.value_map ~default:[] ~f:(List.append edits))
    ;;

    let add_remove t label instr =
      Ident.Table.add_multi t ~key:label ~data:(Edit.remove instr);
      ()
    ;;

    let add_replace t label instr =
      add_remove t label instr;
      add_insert t label instr;
      ()
    ;;

    let apply_blocks ?no_sort t blocks =
      Ident.Map.map blocks ~f:(fun block ->
        let edit = Ident.Table.find_multi t block.Block.label |> List.rev in
        Edit.apply ?no_sort edit block)
    ;;
  end

  module Std = struct
    module Instr = struct
      include Instr
      include Instr_ext
    end

    module Instr' = Instr'
    module Block = Block
    module Adj_map = Adj_map
    module Adj_table = Adj_table
    module Func = Func
    module Edit = Edit
    module Temp_entity = Arg.Temp_entity
    module Temp = Temp_entity.Ident
  end
end
