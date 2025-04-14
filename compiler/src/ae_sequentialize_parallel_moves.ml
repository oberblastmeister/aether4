open Std

module Make (Arg : sig
    module Temp : sig
      type t [@@deriving sexp_of]
    end

    module Ty : sig
      type t [@@deriving sexp_of]
    end
  end) =
struct
  open Arg

  module Move = struct
    type t =
      { dst : Temp.t
      ; src : Temp.t
      ; ty : Ty.t
      }
    [@@deriving sexp_of]
  end

  let sequentialize ~in_same_reg ~get_scratch (moves : Move.t list) =
    let open struct
      type status =
        | To_move
        | Being_moved
        | Moved
      [@@deriving equal]
    end in
    let did_use_scratch = ref false in
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
              did_use_scratch := true;
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
    Lstack.to_list sequential, !did_use_scratch
  ;;
end
