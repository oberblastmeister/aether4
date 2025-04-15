open Std

open struct
  module Entity = Ae_entity_std
  module Generic_ir = Ae_generic_ir_std
end

module T = struct
  include Ae_abs_x86_types0
  include Generic_ir.Make_ir (Ae_abs_x86_types0)
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
end

module Func = struct
  include Func

  let create_stack_builder func =
    { Stack_builder.stack_slot_gen = func.data.next_stack_slot_id
    ; stack_slots = func.data.stack_slots
    }
  ;;

  let apply_stack_builder (stack_builder : Stack_builder.t) func =
    let next_stack_slot_id = stack_builder.stack_slot_gen in
    let stack_slots = stack_builder.stack_slots in
    let data = { func.data with next_stack_slot_id; stack_slots } in
    { func with data }
  ;;
end
