open Std
open Ae_abs_x86_types
module Stack_slot = Ae_stack_slot
open Ae_trace

type t =
  { (* basically a prefix sum of offsets *)
    table : int Stack_slot.Table.t
  ; frame_size : int
  }
[@@deriving sexp_of]

let resolve_frame_base_offset t slot = t.table.Stack_slot.Table.Syntax.!(slot)
let frame_size t = t.frame_size

let compute_bottom_size func =
  let max_offset = ref (-8) in
  begin
    let@: block = Func.iter_blocks func in
    let@: instr = Block.iter_fwd block in
    begin
      match instr.i with
      | Call { args; _ } ->
        let num_on_stack =
          List.length args - Call_conv.num_arguments_in_registers |> max 0
        in
        max_offset := max !max_offset (num_on_stack * 8)
      | _ -> ()
    end;
    let@: operand = Instr.iter_operands instr.i in
    let@: stack_addr = Operand.stack_val operand |> Option.iter in
    let@: offset = Stack_address.current_frame_val stack_addr |> Option.iter in
    max_offset := max !max_offset (Int.of_int32_exn offset)
  end;
  Int.round_up ~to_multiple_of:8 (!max_offset + 8)
;;

let compute (func : Func.t) =
  let offset = ref 0 in
  let layout_table = Stack_slot.Table.create () in
  begin
    let@: slot, size = List.iter (List.rev func.stack_slots) in
    offset := !offset - Int.round_up ~to_multiple_of:8 (Ty.to_bytes size);
    layout_table.Stack_slot.Table.Syntax.!(slot) <- !offset
  end;
  (* since the stack is 16 byte aligned before the call,
     it is 16 byte aligned inside the call,
     due to the return address and base pointer being pushed
  *)
  let align size =
    assert (size % 8 = 0);
    if size % 16 = 8 then size + 8 else size
  in
  let bottom_size = compute_bottom_size func in
  let frame_size = align (Int.neg !offset + bottom_size) in
  trace_s [%message (offset : int ref) (bottom_size : int) (frame_size : int)];
  { table = layout_table; frame_size }
;;
