open Std

open struct
  module Entity = Ae_entity_std
  module Condition_code = Ae_x86_condition_code
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
    | And of
        { dst : Operand.t
        ; src : Operand.t
        ; size : Size.t
        }
    | Or of
        { dst : Operand.t
        ; src : Operand.t
        ; size : Size.t
        }
    | Xor of
        { dst : Operand.t
        ; src : Operand.t
        ; size : Size.t
        }
    | Mov of
        { dst : Operand.t
        ; src : Operand.t
        ; size : Size.t
        }
    | Movzx of
        { dst : Operand.t
        ; dst_size : Size.t
        ; src : Operand.t
        ; src_size : Size.t
        }
    | Sal of
        { dst : Operand.t
        ; size : Size.t
        }
    | Sar of
        { dst : Operand.t
        ; size : Size.t
        }
    | Cmp of
        { src1 : Operand.t
        ; src2 : Operand.t
        ; size : Size.t
        }
    | Test of
        { src1 : Operand.t
        ; src2 : Operand.t
        ; size : Size.t
        }
    | Set of
        { cc : Condition_code.t
        ; dst : Operand.t
        }
    | Lea of
        { dst : Operand.t
        ; src : Address.t
        }
    | Mov_abs of
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
    | Cqo
    | Jmp of string
    | J of
        { cc : Condition_code.t
        ; label : string
        }
    | Ret
  [@@deriving sexp_of, variants]
end

module Line = struct
  type t =
    | Directive of string
    (*
       sign extends RAX and stores the result in registers RDX:RAX
    *)
    | Label of string
    | Comment of string
    | Instr of
        { i : Instr.t
        ; info : Info.t option
        }
  [@@deriving sexp_of, variants]
end

module Program = struct
  type t = Line.t list [@@deriving sexp_of]
end
