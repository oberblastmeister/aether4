open Std

open struct
  module Entity = Ae_entity_std
  module Generic_ir = Ae_generic_ir_std
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
        ; next_temp_id : Temp_entity.Id.t
        ; next_label_id : Label_entity.Id.t
        ; next_stack_slot_id : Stack_slot_entity.Id.t
        ; stack_slots : (Stack_slot.t * Ty.t) list
        }
      [@@deriving sexp_of, fields ~getters ~setters]

      let set_blocks t blocks = { t with blocks }
      let create_temp_gen t = Entity.Id_gen.of_id t.next_temp_id

      let apply_temp_gen temp_gen t =
        { t with next_temp_id = Entity.Id_gen.next temp_gen }
      ;;

      let apply_multi_edit ?no_sort edit t =
        { t with blocks = Multi_edit.apply_blocks ?no_sort edit t.blocks }
      ;;

      let create_label_gen t = Entity.Id_gen.of_id t.next_label_id

      let apply_label_gen label_gen t =
        { t with next_label_id = Entity.Id_gen.next label_gen }
      ;;
    end

    include T
    open Generic_ir.Make_cfg (Block) (T)
    include Func_ext

    let create_stack_builder func =
      { Stack_builder.stack_slot_gen = func.next_stack_slot_id
      ; stack_slots = func.stack_slots
      }
    ;;

    let apply_stack_builder (stack_builder : Stack_builder.t) func =
      let next_stack_slot_id = stack_builder.stack_slot_gen in
      let stack_slots = stack_builder.stack_slots in
      { func with next_stack_slot_id; stack_slots }
    ;;

    let create_mach_reg_gen ?allocation func =
      { Mach_reg_gen.table = Hashtbl.create (module Mach_reg)
      ; temp_gen = func.next_temp_id
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
