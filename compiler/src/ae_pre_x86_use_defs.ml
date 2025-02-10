open Std
module Pre_x86 = Ae_pre_x86_types
module Temp = Pre_x86.Temp

module Operand = struct
  let iter_mem_regs (o : Pre_x86.Operand.t) ~f =
    match o with
    | Reg _ -> ()
    | Imm _ -> ()
    | Mem _addr -> todol [%here]
  ;;

  let iter_any_regs (o : Pre_x86.Operand.t) ~f =
    match o with
    | Reg r ->
      f r;
      ()
    | Imm _ -> ()
    | Mem _addr -> todol [%here]
  ;;

  let iter_reg_val (o : Pre_x86.Operand.t) ~f =
    match o with
    | Reg r ->
      f r;
      ()
    | Imm _ | Mem _ -> ()
  ;;
end

module Instr = struct
  let operand_use_defs (instr : Pre_x86.Instr.t) ~on_def ~on_use =
    match instr with
    | BlockMov { temps } ->
      List.iter temps ~f:(fun temp -> on_def (Pre_x86.Operand.Reg temp))
    | Mov { dst; src } ->
      on_use src;
      on_def dst;
      ()
    | MovAbs { dst; src = _ } ->
      on_def dst;
      ()
    | Add { dst; src1; src2 } | Sub { dst; src1; src2 } ->
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
end
