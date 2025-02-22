open Std
open Ae_generic_ir_sigs

open struct
  module Entity = Ae_entity_std
  module Label_entity = Ae_label_entity
  module Label = Label_entity.Ident
  module Entity_graph_utils = Ae_entity_graph_utils
  module Dominators = Ae_dominators
end

module Make_ir (Arg : Arg) = struct
  open Arg
  module Arg = Arg
  module Temp = Temp_entity.Ident

  module Instr' = struct
    type t =
      { i : Instr.t
      ; index : int
      ; info : Info.t option
      }
    [@@deriving sexp_of]

    let create ?info i = { i; index = -1; info }
    let invalid_nop = { i = Instr.nop; index = -1; info = None }
  end

  module Block = struct
    type t = { body : Instr'.t iarray } [@@deriving sexp_of]

    let iter_fwd t ~f = Arrayp.iter t.body ~f
    let iter_bwd t ~f = Arrayp.iteri_rev t.body ~f:(fun _ x -> f x)

    let of_array body =
      { body = Arrayp.mapi body ~f:(fun index instr -> { instr with Instr'.index }) }
    ;;

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
  end

  module Func = struct
    type t =
      { name : string
      ; blocks : Block.t Label.Map.t
      ; start : Label.t
      ; next_id : Temp_entity.Id.t
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
        Instr.jumps i.i
        |> Option.value_exn
             ~error:(Error.create "should be a jump instruction" i Instr'.sexp_of_t))
    ;;

    let graph func = Entity_graph_utils.graph_of_adj_map (succ_map func)
    let bi_graph func = graph func |> Entity_graph_utils.to_bi

    let compute_idoms ?graph t =
      Dominators.Immediate.compute
        ~start:t.start
        (Option.value_or_thunk graph ~default:(fun () -> bi_graph t))
    ;;
  end

  module Edit = struct
    type t =
      | Insert of (int * Instr.t)
      | Remove of int
    [@@deriving sexp_of]

    let insert i instr = Insert (i, instr)
    let remove i = Remove i

    let apply edit (block : Block.t) =
      let inserts, removes =
        List.partition_map
          ~f:(function
            | Insert x -> First x
            | Remove x -> Second x)
          edit
      in
      let inserts =
        List.map ~f:(fun (ix, i) -> ix, { Instr'.i; index = -1; info = None }) inserts
      in
      let body = Arrayp.to_array block.body in
      List.iter removes ~f:(fun i -> Array.set body i Instr'.invalid_nop);
      let body = Ae_array_utils.apply_inserts Instr'.invalid_nop inserts body in
      let res =
        Array.filter_mapi body ~f:(fun index instr ->
          if Instr.is_nop instr.i then None else Some { instr with index })
      in
      let res = Arrayp.of_array res in
      { block with body = res }
    ;;
  end
end
