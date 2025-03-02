open Std

open struct
  module Entity = Ae_entity_std
  module Generic_ir = Ae_generic_ir_std
end

module Size = Ae_x86_size
module Vreg_entity = Ae_vreg_entity
module Vreg = Vreg_entity.Ident
module Label_entity = Ae_label_entity
module Label = Label_entity.Ident
module Mach_reg = Ae_x86_mach_reg
module Stack_slot_entity = Ae_stack_slot_entity
module Stack_slot = Stack_slot_entity.Ident

module Address = struct
  type t = Vreg.t Ae_x86_address.t [@@deriving sexp_of]
end

module Alloc_reg = struct
  type t =
    | InReg of Mach_reg.t
    | Spilled of Stack_slot.t
  [@@deriving sexp_of, variants]
end

module Operand = struct
  type t =
    | Imm of Int32.t
    | Reg of Vreg.t
    | Mem of Address.t
  [@@deriving sexp_of]
end

module Bin_op = struct
  type t =
    | Add
    | Sub
    | Imul
    | Idiv
    | Imod
  [@@deriving sexp_of]
end

module Block_call = Ae_block_call.Make (Vreg_entity)

module Instr = struct
  type t =
    | Block_params of { temps : (Vreg.t * Size.t) list }
    | Nop
    | Mov of
        { dst : Operand.t
        ; src : Operand.t
        ; size : Size.t
        }
    | MovAbs of
        { dst : Operand.t
        ; src : int64
        }
    | Bin of
        { dst : Operand.t
        ; op : Bin_op.t
        ; src1 : Operand.t
        ; src2 : Operand.t
        }
    | Jump of Block_call.t
    | Cond_jump of
        { cond : Vreg.t
        ; b1 : Block_call.t
        ; b2 : Block_call.t
        }
    | Ret of
        { src : Operand.t
        ; size : Size.t
        }
  [@@deriving sexp_of, variants]

  let nop = Nop

  let is_nop = function
    | Nop -> true
    | _ -> false
  ;;

  let iter_uses _ = todol [%here]
  let iter_defs _ = todol [%here]
  let iter_defs_with_ty _ = todol [%here]
  let get_jumps _ = todol [%here]
  let map_uses _ = todol [%here]
  let map_defs _ = todol [%here]
  let map_block_calls _ = todol [%here]
  let iter_block_calls _ = todol [%here]
end

include Generic_ir.Make_all (struct
    module Block_call = Block_call
    module Instr = Instr
    module Temp_entity = Vreg_entity
    module Ty = Size

    module Func_data = struct
      type t = unit [@@deriving sexp_of]
    end
  end)

module Instr' = Ir.Instr'
module Func = Ir.Func
module Block = Ir.Block
module Edit = Ir.Edit
