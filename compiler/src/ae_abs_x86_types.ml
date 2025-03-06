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
    | Imul
    | Idiv
    | Imod
  [@@deriving sexp_of]
end

module Block_call = Ae_block_call.Make (Vreg_entity)

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

module Instr = struct
  type t =
    | Block_params of { temps : (Vreg.t * Size.t) list }
    | Nop
    | Mov of
        { dst : Operand.t
        ; src : Operand.t
        ; size : Size.t
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
        ; size : Size.t
        }
    | Push of { src : Operand.t }
    | Pop of { dst : Operand.t }
  [@@deriving sexp_of, variants]

  let nop = Nop

  let is_nop = function
    | Nop -> true
    | _ -> false
  ;;

  let iter_operand_use_defs (instr : t) ~on_def ~on_use =
    match instr with
    | Nop -> ()
    | Push { src } ->
      on_use src;
      ()
    | Pop { dst } ->
      on_def dst;
      ()
    | Block_params { temps } ->
      List.iter temps ~f:(fun (temp, _size) -> on_def (Operand.Reg temp))
    | Jump b ->
      Block_call.iter_uses b ~f:(fun r -> on_use (Reg r));
      ()
    | Cond_jump { cond; b1; b2 } ->
      (match cond with
       | Op o -> on_use o
       | Bin { src1; op = _; src2 } ->
         on_use src1;
         on_use src2);
      Block_call.iter_uses b1 ~f:(fun r -> on_use (Reg r));
      Block_call.iter_uses b2 ~f:(fun r -> on_use (Reg r));
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
  ;;

  let map_operand_use_defs (instr : t) ~on_def ~on_use =
    let map_temp temp ~f =
      f (Operand.Reg temp)
      |> Operand.reg_val
      |> Option.value_exn ~message:"Mapper should not change register operand kind"
    in
    match instr with
    | Nop -> instr
    | Push { src } ->
      let src = on_use src in
      Push { src }
    | Pop { dst } ->
      let dst = on_def dst in
      Pop { dst }
    | Block_params { temps } ->
      let temps = (List.map & Tuple2.map_fst) temps ~f:(map_temp ~f:on_def) in
      Block_params { temps }
    | Jump bc ->
      let bc = Block_call.map_uses bc ~f:(map_temp ~f:on_use) in
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
      let b1 = Block_call.map_uses b1 ~f:(map_temp ~f:on_use) in
      let b2 = Block_call.map_uses b2 ~f:(map_temp ~f:on_use) in
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
  ;;

  let iter_uses instr ~f =
    iter_operand_use_defs
      instr
      ~on_def:(fun o -> Operand.iter_mem_regs o ~f)
      ~on_use:(fun o -> Operand.iter_any_regs o ~f)
  ;;

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
       | Add | Sub -> ()
       | Imul | Idiv | Imod ->
         f Mach_reg.RAX;
         f Mach_reg.RDX;
         ())
    | Nop | Push _ | Pop _ | Block_params _ | Jump _ | Cond_jump _ | Mov _ | Mov_abs _ ->
      ()
    | Ret _ -> f Mach_reg.RAX
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
    | Jump _ | Cond_jump _ | Ret _ -> true
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

include Generic_ir.Make_all (struct
    module Block_call = Block_call
    module Instr = Instr
    module Temp_entity = Vreg_entity
    module Ty = Size

    module Func_data = struct
      type t = unit [@@deriving sexp_of]
    end
  end)

module Destruct_ssa = Generic_ir.Destruct_ssa.Make (Ir) (Instr)
include Ir
