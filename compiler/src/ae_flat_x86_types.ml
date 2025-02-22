open Std

open struct
  module Entity = Ae_entity_std
end

module Mach_reg = Ae_x86_mach_reg

module Address = struct
  type t = Mach_reg.t Ae_x86_address.t [@@deriving sexp_of]
end

module Size = Ae_x86_size

module Operand = struct
  type t =
    | Imm of Int32.t
    | Reg of Mach_reg.t
    | Mem of Address.t
  [@@deriving sexp_of]
end

module Instr = struct
  type t =
    | Add of
        { dst : Operand.t
        ; src : Operand.t
        ; size : Size.t
        }
    | Sub of
        { dst : Operand.t
        ; src : Operand.t
        ; size : Size.t
        }
    | Imul of
        { src : Operand.t
        ; size : Size.t
        }
    | Idiv of
        { src : Operand.t
        ; size : Size.t
        }
    | Mov of
        { dst : Operand.t
        ; src : Operand.t
        ; size : Size.t
        }
    | Lea of
        { dst : Operand.t
        ; src : Address.t
        }
    | MovAbs of
        { dst : Operand.t
        ; src : int64
        }
    | Push of
        { src : Operand.t
        ; size : Size.t
        }
    | Pop of
        { dst : Operand.t
        ; size : Size.t
        }
    | Ret
    | Directive of string
    (*
       sign extends RAX and stores the result in registers RDX:RAX
    *)
    | Cqo
    | Label of string
    | Comment of string
  [@@deriving sexp_of]
end

module Program = struct
  type t = Instr.t list [@@deriving sexp_of]
end
