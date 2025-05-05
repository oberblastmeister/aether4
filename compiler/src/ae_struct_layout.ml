open Std

module Field = struct
  type t =
    { size : int
    ; align : int
    }
  [@@deriving sexp_of]
end

type t =
  { offsets : int iarray
  ; size : int
  ; align : int
  }
[@@deriving sexp_of]

let calculate fields =
  let offset = ref 0 in
  let offsets = Arrayp.create ~len:(List.length fields) 0 in
  let align = ref 0 in
  begin
    let@: i, field = List.iteri fields |> Iter.uncurry in
    let off = Int.round_up !offset ~to_multiple_of:field.Field.align in
    offsets.@(i) <- off;
    align := max !align field.align;
    offset := off + field.size;
    ()
  end;
  let size = Int.round_up !offset ~to_multiple_of:!align in
  let align = !align in
  { offsets = Arrayp.copy offsets; size; align }
;;
