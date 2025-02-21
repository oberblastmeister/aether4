open Std

open struct
  module Entity = Ae_entity_std
end

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

module Instr = struct
  type t =
    | BlockMov of { temps : Vreg.t list }
    | Mov of
        { dst : Operand.t
        ; src : Operand.t
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
    | Ret of { src : Operand.t }
  [@@deriving sexp_of]
end

module Block = struct
  type t = { body : Instr.t list } [@@deriving sexp_of]

  let iter_instrs_backwards block ~f = List.rev block.body |> List.iter ~f
end

module Func = struct
  type t =
    { name : string
    ; blocks : Block.t Label.Map.t
    ; start : Label.t
    ; next_id : Vreg_entity.Id.t
    }
  [@@deriving sexp_of]

  let get_start_block func =
    Entity.Ident.Map.find func.blocks func.start
    |> Option.value_or_thunk ~default:(fun () ->
      raise_s
        [%message "invariant broken: start block did not exist" (func.start : Label.t)])
  ;;
end
