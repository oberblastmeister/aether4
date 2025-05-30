open Std

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
  module Graph = Ae_data_graph_std
end

module type Block = sig
  type t [@@deriving sexp_of]

  val get_succ : t -> Label.t list
end

module Make_cfg_S (Block : Block) (Func : T) = struct
  module type S = sig
    module Adj_map : sig
      type t = Label.t list Label.Map.t
    end

    module Adj_table : sig
      type t = Label.t list Label.Table.t
    end

    module Func_ext : sig
      type t := Func.t

      val start_block : t -> Block.t
      val succ_map : t -> Adj_map.t
      val iter_blocks : t -> Block.t Iter.t
      val find_block_exn : t -> Label.t -> Block.t
      val pred_table : t -> Adj_table.t
      val pred_table_of_succ : Adj_table.t -> Adj_table.t
      val succ_table : t -> Label.t list Label.Table.t
      val bi_graph : t -> Label.t Graph.Bi.t
      val graph : t -> Label.t Graph.t
      val compute_idoms : ?graph:Label.t Graph.Bi.t -> t -> Dominators.t
      val compute_dom_tree : ?graph:Label.t Graph.Bi.t -> t -> Dominators.Tree.t
      val labels_postorder : t -> Label.t Vec.t
      val labels_reverse_postorder : t -> Label.t Vec.t
    end
  end
end

module Make
    (Block : Block)
    (Func : sig
       type t

       val start : t -> Label.t
       val blocks : t -> Block.t Label.Map.t
       val set_blocks : t -> Block.t Label.Map.t -> t
     end) : Make_cfg_S(Block)(Func).S = struct
  module Adj_map = struct
    type t = Label.t list Label.Map.t
  end

  module Adj_table = struct
    type t = Label.t list Label.Table.t
  end

  module Func_ext = struct
    type t = Func.t

    let start_block func =
      let start = Func.start func in
      let blocks = Func.blocks func in
      Entity.Ident.Map.find blocks start
      |> Option.value_or_thunk ~default:(fun () ->
        raise_s [%message "invariant broken: start block did not exist" (start : Label.t)])
    ;;

    let find_block_exn func label = Entity.Ident.Map.find_exn (Func.blocks func) label
    let succ_map func = Func.blocks func |> Entity.Ident.Map.map ~f:Block.get_succ

    let succ_list func =
      Func.blocks func
      |> Entity.Ident.Map.to_alist
      |> List.map ~f:(fun (lab, b) -> lab, Block.get_succ b)
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
        ~start:(Func.start t)
        (Option.value_or_thunk graph ~default:(fun () -> bi_graph t))
    ;;

    let compute_dom_tree ?graph t =
      let idoms = compute_idoms ?graph t in
      Dominators.Tree.of_immediate idoms
    ;;

    let iter_blocks t = Func.blocks t |> Ident.Map.iter

    let labels_reverse_postorder func =
      Label_entity.Dfs.reverse_postorder ~start:[ Func.start func ] (graph func)
    ;;

    let labels_postorder func =
      Label_entity.Dfs.postorder ~start:[ Func.start func ] (graph func)
    ;;
  end
end
