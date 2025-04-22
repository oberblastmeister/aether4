(* TODO: change this to make edit and make Instr' and Block abstract in generic_ir_sigs *)
open Std
open Ae_generic_ir_sigs
module Graph = Ae_data_graph_std

open struct
  module Intf = Ae_generic_ir_make_intf
  module Label = Ae_label

  (* 
  
  module Entity_graph_utils = Ae_entity_graph_utils
  module Label = Ae_label *)
  module Dominators = Ae_dominators
  module Stack_slot = Ae_stack_slot
  (* module Id_gen = Entity.Id_gen *)
end

module Make (Arg : Intf.Arg) : Intf.S with module Arg := Arg = struct
  module Arg = Arg

  (* very important! *)
  (* the open Arg must be after the module alias *)
  open Arg

  module Instr' = struct
    module T = struct
      type t =
        { i : Instr.t
        ; index : int
        ; info : Info.t option
        ; ann : Ann.t
        }
      [@@deriving sexp_of]

      let to_int t = t.index
    end

    include T

    let instr t = t.i
    let map t ~f = { t with i = f t.i }
    let create ?(ann = Ann.default) ?info i index = { i; index; info; ann }
    let create_unindexed ?(ann = Ann.default) ?info i = { i; index = -1; info; ann }
    let invalid_nop = { i = Instr.nop; index = -1; info = None; ann = Ann.default }

    (* module Table = Entity.Table.Make (T)
    module Map = Entity.Map.Make (T) *)
  end

  module Block = struct
    module T = struct
      type t =
        { label : Label.t
        ; body : Instr'.t iarray
        }
      [@@deriving sexp_of]

      (* let to_int t = Entity.Id.to_int t.label.id *)
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

    let get_succ t =
      let i = find_control t in
      Instr.iter_block_calls i.i |> Iter.map ~f:Block_call.label |> Iter.to_list
    ;;

    (* module Table = Entity.Table.Make (T)
    module Map = Entity.Map.Make (T) *)
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

    let create () = Label.Table.create ()

    let add_insert t label instr =
      Label.Table.add_multi t ~key:label ~data:(Edit.insert instr);
      ()
    ;;

    let add_edits t label edits =
      Label.Table.update t label ~f:(function
        | None -> List.rev edits
        | Some es -> List.rev_append edits es)
    ;;

    let add_inserts t label inserts = add_edits t label @@ List.map inserts ~f:Edit.insert

    let add_remove t label instr =
      Label.Table.add_multi t ~key:label ~data:(Edit.remove instr);
      ()
    ;;

    let add_replace t label instr =
      Label.Table.add_multi t ~key:label ~data:(Edit.replace instr);
      ()
    ;;

    let apply_blocks ?no_sort t blocks =
      Label.Table.iteri t
      |> Iter.fold ~init:blocks ~f:(fun blocks (label, edits) ->
        let edits = List.rev edits in
        let block =
          Map.find blocks label
          |> Option.value_or_thunk ~default:(fun () ->
            Block.create label (Arrayp.of_list []))
        in
        let block = Edit.apply ?no_sort edits block in
        Map.set blocks ~key:label ~data:block)
    ;;
  end
end
