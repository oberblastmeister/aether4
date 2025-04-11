open Std

open struct
  module Entity = Ae_entity_std
  module Condition_code = Ae_x86_condition_code
end

module Mach_reg = Ae_x86_mach_reg

module Address = struct
  type t = Mach_reg.t Ae_x86_address.t [@@deriving sexp_of]
end

module Ty = Ae_x86_ty

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
        ; size : Ty.t
        }
    | Sub of
        { dst : Operand.t
        ; src : Operand.t
        ; size : Ty.t
        }
    | Imul of
        { src : Operand.t
        ; size : Ty.t
        }
    | Idiv of
        { src : Operand.t
        ; size : Ty.t
        }
    | And of
        { dst : Operand.t
        ; src : Operand.t
        ; size : Ty.t
        }
    | Or of
        { dst : Operand.t
        ; src : Operand.t
        ; size : Ty.t
        }
    | Xor of
        { dst : Operand.t
        ; src : Operand.t
        ; size : Ty.t
        }
    | Mov of
        { dst : Operand.t
        ; src : Operand.t
        ; size : Ty.t
        }
    | Movzx of
        { dst : Operand.t
        ; dst_size : Ty.t
        ; src : Operand.t
        ; src_size : Ty.t
        }
    | Sal of
        { dst : Operand.t
        ; size : Ty.t
        }
    | Sar of
        { dst : Operand.t
        ; size : Ty.t
        }
    | Cmp of
        { src1 : Operand.t
        ; src2 : Operand.t
        ; size : Ty.t
        }
    | Test of
        { src1 : Operand.t
        ; src2 : Operand.t
        ; size : Ty.t
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
        ; size : Ty.t
        }
    | Pop of
        { dst : Operand.t
        ; size : Ty.t
        }
    | Cqo
    | Jmp of string
    | J of
        { cc : Condition_code.t
        ; label : string
        }
    | Ret
    | Call of string
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
