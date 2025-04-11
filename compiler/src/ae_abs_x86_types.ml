open Std

open struct
  module Entity = Ae_entity_std
  module Generic_ir = Ae_generic_ir_std
end

module Ty = Ae_x86_ty
module Temp_entity = Ae_abs_asm_temp_entity
module Temp = Temp_entity.Ident
module Label_entity = Ae_label_entity
module Label = Label_entity.Ident
module Mach_reg = Ae_x86_mach_reg
module Stack_slot_entity = Ae_stack_slot_entity
module Stack_slot = Stack_slot_entity.Ident
module Call_conv = Ae_x86_call_conv

module Address = struct
  type t = Temp.t Ae_x86_address.t [@@deriving sexp_of]
end

module Operand = struct
  type t =
    | Imm of Int32.t
    | Reg of Temp.t
    | Mem of Address.t
    | Stack_slot of Stack_slot.t
  [@@deriving sexp_of, variants]

  let iter_mem_regs (o : t) ~f =
    match o with
    | Reg _ -> ()
    | Stack_slot _ | Imm _ -> ()
    | Mem _addr -> todol [%here]
  ;;

  let iter_any_regs (o : t) ~f =
    match o with
    | Reg r ->
      f r;
      ()
    | Stack_slot _ | Imm _ -> ()
    | Mem _addr -> todol [%here]
  ;;

  let iter_reg_val (o : t) ~f =
    match o with
    | Reg r ->
      f r;
      ()
    | Stack_slot _ | Imm _ | Mem _ -> ()
  ;;

  let map_reg o ~f =
    match o with
    | Reg r -> Reg (f r)
    | _ -> o
  ;;
end

module Bin_op = struct
  type t =
    | Add
    | Sub
    (* use the two operand form *)
    | Imul
    | Idiv
    | Imod
    | Lt
    | Gt
    | Le
    | Ge
    | And of Ty.t
    | Or of Ty.t
    | Xor of Ty.t
    | Eq of Ty.t
    | Lshift
    | Rshift
  [@@deriving sexp_of]
end

module Location = struct
  type t =
    | Temp of Temp.t
    | Slot of Stack_slot.t
  [@@deriving sexp_of, variants]

  let temp_val = function
    | Temp temp -> Some temp
    | Slot _ -> None
  ;;

  let iter_temp t ~f =
    match t with
    | Temp temp -> f temp
    | Slot _ -> ()
  ;;

  let map_temp t ~f =
    match t with
    | Temp temp -> Temp (f temp)
    | Slot _ -> t
  ;;

  let to_operand = function
    | Temp v -> Operand.Reg v
    | Slot s -> Operand.Stack_slot s
  ;;

  let of_temp t = Temp t
end

module Block_call = Ae_block_call.Make (Location)

module Cond_expr = struct
  type t =
    | Op of Operand.t
    | Bin of
        { src1 : Operand.t
        ; op : Bin_op.t
        ; src2 : Operand.t
        }
  [@@deriving sexp_of]
end

module Block_param = struct
  type t =
    { param : Location.t
    ; ty : Ty.t
    }
  [@@deriving sexp_of, fields]
end

module Instr = struct
  type t =
    | Block_params of Block_param.t list
    | Nop
    | Mov of
        { dst : Operand.t
        ; src : Operand.t
        ; size : Ty.t
        }
    | Mov_abs of
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
        { cond : Cond_expr.t
        ; b1 : Block_call.t
        ; b2 : Block_call.t
        }
    | Ret of
        { src : Operand.t
        ; size : Ty.t
        }
    | Call of
        { dst : Temp.t
        ; size : Ty.t
        ; args : Temp.t list
        }
    | Unreachable
  [@@deriving sexp_of, variants]

  let empty_block_params = Block_params []

  let block_params_tys = function
    | Block_params params -> Some (List.map ~f:Block_param.ty params)
    | _ -> None
  ;;

  let nop = Nop

  let is_nop = function
    | Nop -> true
    | _ -> false
  ;;

  let iter_operand_use_defs (instr : t) ~on_def ~on_use =
    match instr with
    | Nop -> ()
    | Call { dst; size = _; args } ->
      on_def (Operand.Reg dst);
      List.iter args ~f:(fun temp -> on_use (Operand.Reg temp))
    | Block_params params ->
      (List.iter @> Fold.of_fn Block_param.param @> Location.iter_temp)
        params
        ~f:(fun temp -> on_def (Operand.Reg temp))
    | Jump b ->
      (Block_call.iter_uses @> Location.iter_temp) b ~f:(fun r -> on_use (Operand.Reg r));
      ()
    | Cond_jump { cond; b1; b2 } ->
      (match cond with
       | Op o -> on_use o
       | Bin { src1; op = _; src2 } ->
         on_use src1;
         on_use src2);
      (Block_call.iter_uses @> Location.iter_temp) b1 ~f:(fun r -> on_use (Reg r));
      (Block_call.iter_uses @> Location.iter_temp) b2 ~f:(fun r -> on_use (Reg r));
      ()
    | Mov { dst; src; size = _ } ->
      on_use src;
      on_def dst;
      ()
    | Mov_abs { dst; src = _ } ->
      on_def dst;
      ()
    | Bin { dst; src1; op = _; src2 } ->
      on_use src1;
      on_use src2;
      on_def dst;
      ()
    | Ret { src; size = _ } ->
      on_use src;
      ()
    | Unreachable -> ()
  ;;

  let map_operand_use_defs (instr : t) ~on_def ~on_use =
    let map_temp temp ~f =
      f (Operand.Reg temp)
      |> Operand.reg_val
      |> Option.value_exn ~message:"Mapper should not change register operand kind"
    in
    match instr with
    | Nop -> instr
    | Call p ->
      let args = List.map p.args ~f:(map_temp ~f:on_def) in
      Call { p with args }
    | Block_params params ->
      let mapper =
        List.map & Traverse.of_field Block_param.Fields.param & Location.map_temp
      in
      let params = mapper params ~f:(fun x -> map_temp ~f:on_def x) in
      Block_params params
    | Jump bc ->
      let bc = (Block_call.map_uses & Location.map_temp) bc ~f:(map_temp ~f:on_use) in
      Jump bc
    | Cond_jump { cond; b1; b2 } ->
      let cond : Cond_expr.t =
        match cond with
        | Op cond ->
          let cond = on_use cond in
          Op cond
        | Bin { src1; op; src2 } ->
          let src1 = on_use src1 in
          let src2 = on_use src2 in
          Bin { src1; op; src2 }
      in
      let b1 = (Block_call.map_uses & Location.map_temp) b1 ~f:(map_temp ~f:on_use) in
      let b2 = (Block_call.map_uses & Location.map_temp) b2 ~f:(map_temp ~f:on_use) in
      Cond_jump { cond; b1; b2 }
    | Mov { dst; src; size } ->
      let src = on_use src in
      let dst = on_def dst in
      Mov { dst; src; size }
    | Mov_abs { dst; src } ->
      let dst = on_def dst in
      Mov_abs { dst; src }
    | Bin { dst; src1; op; src2 } ->
      let src1 = on_use src1 in
      let src2 = on_use src2 in
      let dst = on_def dst in
      Bin { dst; src1; op; src2 }
    | Ret { src; size } ->
      let src = on_use src in
      Ret { src; size }
    | Unreachable -> Unreachable
  ;;

  let iter_uses instr ~f =
    iter_operand_use_defs
      instr
      ~on_def:(fun o -> Operand.iter_mem_regs o ~f)
      ~on_use:(fun o -> Operand.iter_any_regs o ~f)
  ;;

  let iter_constrained_uses t ~f =
    match t with
    | Bin { dst = _; op; src1; src2 = _ } ->
      (match op with
       | Idiv | Imod ->
         f (src1, Mach_reg.RAX);
         ()
       | _ -> ())
    | _ -> ()
  ;;

  let iter_constrained_defs t ~f =
    match t with
    | Bin { dst; op; src1 = _; src2 = _ } ->
      (match op with
       | Idiv | Imod ->
         f (dst, Mach_reg.RAX);
         ()
       (* for Imul we can use the two operand version *)
       | _ -> ())
    | _ -> ()
  ;;

  let iter_clobbers t ~f =
    match t with
    | Call _ -> todo ()
    | _ -> ()
  ;;

  let iter_uses_with_known_ty instr ~f = todol [%here]

  let iter_defs instr ~f =
    iter_operand_use_defs
      instr
      ~on_def:(fun o -> Operand.iter_reg_val o ~f)
      ~on_use:(Fn.const ())
  ;;

  let iter_mach_reg_defs (instr : t) ~f =
    match instr with
    | Bin { op; _ } ->
      (match op with
       | Add | Sub | Lt | Gt | Le | Ge | And _ | Or _ | Xor _ | Eq _ -> ()
       | Lshift | Rshift ->
         f Mach_reg.RCX;
         ()
       | Imul | Idiv | Imod ->
         f Mach_reg.RAX;
         f Mach_reg.RDX;
         ())
    | Call _ ->
      List.iter Call_conv.caller_saved_without_r11 ~f;
      ()
    | Nop | Block_params _ | Jump _ | Cond_jump _ | Mov _ | Mov_abs _ -> ()
    | Ret _ -> f Mach_reg.RAX
    | Unreachable -> ()
  ;;

  let iter_defs_with_ty _ = todol [%here]
  let get_jumps _ = todol [%here]

  let map_uses instr ~f =
    map_operand_use_defs
      instr
      ~on_def:(function
        | Mem _m -> todol [%here]
        | o -> o)
      ~on_use:(function
        | Mem _m -> todol [%here]
        | Reg r -> Reg (f r)
        | o -> o)
  ;;

  let map_defs instr ~f =
    map_operand_use_defs
      instr
      ~on_def:(function
        | Reg r -> Reg (f r)
        | o -> o)
      ~on_use:Fn.id
  ;;

  let is_control = function
    | Unreachable | Jump _ | Cond_jump _ | Ret _ -> true
    | _ -> false
  ;;

  let map_block_calls (instr : t) ~f =
    match instr with
    | Jump b -> Jump (f b)
    | Cond_jump { cond; b1; b2 } ->
      let b1 = f b1 in
      let b2 = f b2 in
      Cond_jump { cond; b1; b2 }
    | _ -> instr
  ;;

  let iter_block_calls instr ~f =
    match instr with
    | Ret _ -> ()
    | Jump b ->
      f b;
      ()
    | Cond_jump { cond = _; b1; b2 } ->
      f b1;
      f b2;
      ()
    | _ -> ()
  ;;

  let move ~dst ~src ~ty:size = Mov { dst = Reg dst; src = Reg src; size }
end

module Func_data = struct
  type t =
    { next_stack_slot_id : Stack_slot_entity.Id.t
    ; stack_slots : (Stack_slot.t * Ty.t) list
    }
  [@@deriving sexp_of]
end

include Generic_ir.Make_all (struct
    module Block_param = Block_param
    module Block_call = Block_call
    module Location = Location
    module Instr = Instr
    module Temp_entity = Temp_entity
    module Ty = Ty
    module Func_data = Func_data
  end)

include Ir
