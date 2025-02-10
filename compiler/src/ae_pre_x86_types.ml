open Std
module Entity = Ae_entity_std
module Temp_entity = Entity.Make ()
module Temp = Temp_entity.Name
module Label_entity = Ae_label_entity
module Label = Label_entity.Name

module Address : sig
  module Index : sig
    type t =
      { index : Temp.t
      ; scale : int (* 1, 2, 4, 8 *)
      }
    [@@deriving sexp_of]
  end

  module Base : sig
    type t =
      | Reg of Temp.t
      | Rsp
    [@@deriving sexp_of, variants]
  end

  type t =
    { base : Base.t option
    ; index : Index.t option
    ; offset : int
    }
  [@@deriving sexp_of]

  val create : ?base:Temp.t -> ?index:Index.t -> int -> t
end = struct
  module Index = struct
    type t =
      { index : Temp.t
      ; scale : int
      }
    [@@deriving sexp_of]
  end

  module Base = struct
    type t =
      | Reg of Temp.t
      | Rsp
    [@@deriving sexp_of, variants]
  end

  type t =
    { base : Base.t option
    ; index : Index.t option
    ; offset : int
    }
  [@@deriving sexp_of]

  let create ?base ?index offset = { base = Option.map ~f:Base.reg base; index; offset }
end

module Operand = struct
  type t =
    | Imm of Int32.t
    | Reg of Temp.t
    | Mem of Address.t
  [@@deriving sexp_of]
end

module Instr = struct
  type t =
    | BlockMov of { temps : Temp.t list }
    | Mov of
        { dst : Operand.t
        ; src : Operand.t
        }
    | MovAbs of
        { dst : Operand.t
        ; src : int64
        }
    | Add of
        { dst : Operand.t
        ; src1 : Operand.t
        ; src2 : Operand.t
        }
    | Sub of
        { dst : Operand.t
        ; src1 : Operand.t
        ; src2 : Operand.t
        }
    | Ret of { src : Operand.t }
  [@@deriving sexp_of]
end

module Block = struct
  type t = { body : Instr.t list } [@@deriving sexp_of]
end

module Func = struct
  type t =
    { name : string
    ; blocks : Block.t Label.Map.t
    ; start : Label.t
    ; next_id : Temp_entity.Id.t
    }
  [@@deriving sexp_of]
end
