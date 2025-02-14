open Std
module Abs_x86 = Ae_abs_x86_types
open Abs_x86
module Vreg = Abs_x86.Vreg
module Mach_reg = Ae_x86_mach_reg

module Operand = struct
  let iter_mem_regs (o : Abs_x86.Operand.t) ~f =
    match o with
    | Reg _ -> ()
    | Imm _ -> ()
    | Mem _addr -> todol [%here]
  ;;

  let iter_any_regs (o : Abs_x86.Operand.t) ~f =
    match o with
    | Reg r ->
      f r;
      ()
    | Imm _ -> ()
    | Mem _addr -> todol [%here]
  ;;

  let iter_reg_val (o : Abs_x86.Operand.t) ~f =
    match o with
    | Reg r ->
      f r;
      ()
    | Imm _ | Mem _ -> ()
  ;;
end

module Instr = struct
  let operand_use_defs (instr : Abs_x86.Instr.t) ~on_def ~on_use =
    match instr with
    | BlockMov { temps } ->
      List.iter temps ~f:(fun temp -> on_def (Abs_x86.Operand.Reg temp))
    | Mov { dst; src } ->
      on_use src;
      on_def dst;
      ()
    | MovAbs { dst; src = _ } ->
      on_def dst;
      ()
    | Bin { dst; src1; op = _; src2 } ->
      on_use src1;
      on_use src2;
      on_def dst;
      ()
    | Ret { src } ->
      on_use src;
      ()
  ;;

  let iter_uses instr ~f =
    operand_use_defs
      instr
      ~on_def:(fun o -> Operand.iter_mem_regs o ~f)
      ~on_use:(fun o -> Operand.iter_any_regs o ~f)
  ;;

  let iter_defs instr ~f =
    operand_use_defs
      instr
      ~on_def:(fun o -> Operand.iter_reg_val o ~f)
      ~on_use:(Fn.const ())
  ;;

  let iter_mach_reg_defs (instr : Instr.t) ~f =
    match instr with
    | Bin { op; _ } ->
      (match op with
       | Add | Sub -> ()
       | Imul | Idiv | Imod ->
         f Mach_reg.RAX;
         f Mach_reg.RDX;
         ())
    | Instr.BlockMov _ | Instr.Mov _ | Instr.MovAbs _ -> ()
    | Instr.Ret _ -> f Mach_reg.RAX
  ;;
end
