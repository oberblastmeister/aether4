open Std

open struct
  module Generic_ir = Ae_generic_ir_std
  module Call_conv = Ae_call_conv
end

module T0 = struct
  include Ae_abs_x86_types0
  include Generic_ir.Make_basic_block (Ae_abs_x86_types0)

  module Mach_reg_to_temp = struct
    type t
  end

  module Func = struct
    (*
       TODO: make the temp gen have an expected temp_id so that we can enforce having only one borrow at a time
    *)
    module T = struct
      type t =
        { name : string
        ; blocks : Block.t Label.Map.t
        ; start : Label.t
        ; next_temp_id : int
        ; next_label_id : int
        ; next_stack_slot_id : int
        ; stack_slots : (Stack_slot.t * Ty.t) list
        ; call_conv : Call_conv.t
        }
      [@@deriving sexp_of, fields ~getters ~setters]

      let set_blocks t blocks = { t with blocks }
      let create_temp_gen t = Temp.Id_gen.create t.next_temp_id

      let apply_temp_gen ?renumber temp_gen t =
        if Option.is_none renumber
        then assert (t.next_temp_id = Temp.Id_gen.first_id temp_gen);
        { t with next_temp_id = Temp.Id_gen.get temp_gen }
      ;;

      let apply_multi_edit ?no_sort edit t =
        { t with blocks = Multi_edit.apply_blocks ?no_sort edit t.blocks }
      ;;

      let create_label_gen t = Label.Id_gen.create t.next_label_id

      let apply_label_gen ?renumber label_gen t =
        if Option.is_none renumber
        then assert (t.next_label_id = Label.Id_gen.first_id label_gen);
        { t with next_label_id = Label.Id_gen.get label_gen }
      ;;
    end

    include T
    open Generic_ir.Make_cfg (Block) (T)
    include Func_ext

    let create_stack_builder func =
      { Stack_builder.stack_slot_gen = func.next_stack_slot_id
      ; stack_slots = []
      ; first_id = func.next_stack_slot_id
      }
    ;;

    let apply_stack_builder (stack_builder : Stack_builder.t) func =
      assert (stack_builder.first_id = func.next_stack_slot_id);
      let next_stack_slot_id = stack_builder.stack_slot_gen + 1 in
      let stack_slots = stack_builder.stack_slots in
      { func with
        next_stack_slot_id
      ; stack_slots = List.rev_append stack_slots func.stack_slots
      }
    ;;

    let create_mach_reg_gen ?allocation func =
      { Mach_reg_gen.table = Hashtbl.create (module Mach_reg)
      ; id = func.next_temp_id
      ; allocation
      }
    ;;
  end

  module Program = struct
    type t = { funcs : Func.t list } [@@deriving sexp_of]
  end
end

module T = struct
  open Generic_ir.Make_simple_ext (T0)
  include T0

  module Instr = struct
    include Instr
    include Instr_ext
  end

  module Func = struct
    include Func
    include Func_ext
  end
end

include T
module Liveness = Generic_ir.Liveness.Make (T)
module Check_ssa = Generic_ir.Check_ssa.Make (T)
module Split_critical = Generic_ir.Split_critical.Make (T)
module Normalize_block_params = Generic_ir.Normalize_block_params.Make (T)
module Check_well_formed = Generic_ir.Check_well_formed.Make (T)
module Check_types = Generic_ir.Check_types.Make (T)
module Convert_ssa = Generic_ir.Convert_ssa.Make (T)

module Check = struct
  let check func =
    let open Or_error.Let_syntax in
    let%bind () = Check_well_formed.check func in
    let%bind () = Check_ssa.check func in
    let%bind () = Check_types.check func in
    Ok ()
  ;;

  let check_program (program : Program.t) =
    let open Or_error.Let_syntax in
    let rec go program =
      match program with
      | func :: funcs ->
        let%bind () = check func in
        go funcs
      | [] -> return ()
    in
    go program.funcs
  ;;
end
