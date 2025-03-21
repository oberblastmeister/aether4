open Std
open Ae_generic_ir_import

module Make
    (Ir : Ir)
    (Instr_ext : sig
       open Ir.Std

       val move : dst:Temp.t -> src:Temp.t -> ty:Ty.t -> Instr.t
     end) =
struct
  open Ir.Std

  module Move = struct
    type t =
      { dst : Temp.t
      ; src : Temp.t
      ; ty : Ty.t
      }
    [@@deriving sexp_of]

    let to_instr { dst; src; ty } = Instr_ext.move ~dst ~src ~ty
  end

  let sequentialize_parallel_moves ~in_same_reg ~get_scratch (moves : Move.t list) =
    let open struct
      type status =
        | To_move
        | Being_moved
        | Moved
      [@@deriving equal]
    end in
    let par_move = Array.of_list moves in
    let n = Array.length par_move in
    let status = Array.init n ~f:(Fn.const To_move) in
    let sequential = Lstack.create () in
    let rec move_one i =
      (* self moves don't do anything, so skip them *)
      if in_same_reg par_move.(i).src par_move.(i).dst
      then Lstack.push sequential par_move.(i)
      else begin
        (* if we see Being_moved in the children then we found the unique cycle *)
        status.(i) <- Being_moved;
        (* visit children *)
        for j = 0 to n - 1 do
          (* found a child move whose source will be overwritten by the current move's destination *)
          if in_same_reg par_move.(j).src par_move.(i).dst
          then begin
            match status.(j) with
            | To_move -> move_one j
            | Being_moved ->
              (* unique cycle! *)
              let scratch = get_scratch () in
              Lstack.push
                sequential
                { Move.dst = scratch; src = par_move.(j).src; ty = par_move.(j).ty };
              (* j now should move from the temp because we are about to overwrite j below *)
              par_move.(j) <- { (par_move.(j)) with src = scratch }
            | Moved -> ()
          end
        done;
        (* move ourselves after all the children have been moved *)
        Lstack.push sequential par_move.(i);
        status.(i) <- Moved
      end
    in
    (* make sure all components are traversed *)
    for i = 0 to n - 1 do
      if equal_status status.(i) To_move then move_one i
    done;
    Lstack.to_list sequential
  ;;

  let destruct ~in_same_reg ~get_scratch (func : Func.t) =
    let edit = Multi_edit.create () in
    begin
      let@: block = Func.iter_blocks func in
      let jump_instr = Block.find_control block in
      let num_block_calls = Instr.iter_block_calls jump_instr.i |> Iter.length in
      let jump_instr =
        let@: block_call = (Instr'.map & Instr.map_block_calls) jump_instr in
        let dst_block = Func.find_block_exn func block_call.label in
        let dst_block_params_instr = Block.find_block_params dst_block in
        Multi_edit.add_replace
          edit
          dst_block.label
          { dst_block_params_instr with i = Instr.block_params ~temps:[] };
        let (`temps dst_block_params) =
          Instr.block_params_val dst_block_params_instr.i |> Option.value_exn
        in
        let parallel_moves =
          List.zip_exn dst_block_params block_call.args
          |> List.map ~f:(fun ((dst, ty), src) -> { Move.dst; src; ty })
        in
        let sequential_moves =
          sequentialize_parallel_moves ~in_same_reg ~get_scratch parallel_moves
        in
        (* TODO: test both of these cases *)
        if num_block_calls = 1
        then begin
          Multi_edit.add_inserts
            edit
            block.label
            (List.map sequential_moves ~f:(fun move ->
               Instr'.create (Move.to_instr move) jump_instr.index))
        end
        else begin
          Multi_edit.add_inserts
            edit
            dst_block.label
            (List.map sequential_moves ~f:(fun move ->
               Instr'.create (Move.to_instr move) (dst_block_params_instr.index + 1)))
        end;
        { block_call with args = [] }
      in
      Multi_edit.add_replace edit block.label jump_instr
    end;
    { func with blocks = Multi_edit.apply_blocks ~no_sort:() edit func.blocks }
  ;;
end
