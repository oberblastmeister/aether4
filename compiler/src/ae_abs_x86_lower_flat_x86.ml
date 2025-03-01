open Std
module Abs_x86 = Ae_abs_x86_types
module Entity = Ae_entity_std
module Ident = Entity.Ident
module Flat_x86 = Ae_flat_x86_types
module Bag = Ae_data_bag
module Regalloc = Ae_abs_x86_regalloc
module Mach_reg = Ae_x86_mach_reg

let empty = Bag.empty

open Bag.Syntax

type st =
  { allocation : Regalloc.Allocation.t
  ; mutable label_id : int
  }

let create_state allocation = { allocation; label_id = 0 }

let get_vreg t vreg =
  Ident.Table.find_exn t.allocation vreg
  |> Abs_x86.Alloc_reg.inreg_val
  |> Option.value_or_thunk ~default:(fun () ->
    raise_s
      [%message
        "All spilled registers should have been fixed up with spill instructions"
          (vreg : Abs_x86.Vreg.t)])
;;

let lower_operand st (operand : Abs_x86.Operand.t) : Flat_x86.Operand.t =
  match operand with
  | Imm i -> Imm i
  | Reg vreg -> Reg (get_vreg st vreg)
  | Mem _ -> todol [%here]
;;

let epilogue =
  empty
  +> Flat_x86.Instr.
       [ Mov { dst = Reg RSP; src = Reg RBP; size = Qword }
       ; Pop { dst = Reg RBP; size = Qword }
       ; Ret
       ]
;;

let prologue stack_size =
  empty
  +> Flat_x86.Instr.
       [ Push { src = Reg RBP; size = Qword }
       ; Mov { dst = Reg RBP; src = Reg RSP; size = Qword }
       ; Sub { dst = Reg RSP; src = Imm stack_size; size = Qword }
       ]
;;

let lower_instr st (instr : Abs_x86.Instr.t) : Flat_x86.Instr.t Bag.t =
  match instr with
  | Block_params _ -> empty
  | Nop -> empty
  | Jump _ | Cond_jump _ -> todol [%here]
  | Mov { src; dst; size } ->
    let src = lower_operand st src in
    let dst = lower_operand st dst in
    Flat_x86.Instr.(empty +> [ Mov { src; dst; size } ])
  | MovAbs { dst; src } ->
    let dst = lower_operand st dst in
    Flat_x86.Instr.(empty +> [ MovAbs { dst; src } ])
  | Bin { dst; op; src1; src2 } ->
    let dst = lower_operand st dst in
    let src1 = lower_operand st src1 in
    let src2 = lower_operand st src2 in
    let size = Flat_x86.Size.Qword in
    (match op with
     | Add ->
       Flat_x86.Instr.(
         empty
         +> [ (* ok because src2 is never allocated with scratch *)
              Mov { dst = Reg Mach_reg.scratch; src = src1; size }
            ; Add { dst = Reg Mach_reg.scratch; src = src2; size }
            ; Mov { dst; src = Reg Mach_reg.scratch; size }
            ])
     | Sub ->
       Flat_x86.Instr.(
         empty
         +> [ (* ok because src2 is never allocated with scratch *)
              Mov { dst = Reg Mach_reg.scratch; src = src1; size }
            ; Sub { dst = Reg Mach_reg.scratch; src = src2; size }
            ; Mov { dst; src = Reg Mach_reg.scratch; size }
            ])
     | Imul ->
       Flat_x86.Instr.(
         empty
         +> [ (* must move to scratch first before moving src2 to RAX,
                           because src1 may refer to RAX
              *)
              Mov { dst = Reg Mach_reg.scratch; src = src1; size }
            ; Mov { dst = Reg RAX; src = src2; size }
            ; Imul { src = Reg Mach_reg.scratch; size }
              (* dst here isn't clobbered because in the register allocator we prevent dst from being allocated RAX or RDX *)
            ; Mov { dst; src = Reg RAX; size }
            ])
     | Idiv ->
       Flat_x86.Instr.(
         empty
         +> [ Mov { dst = Reg Mach_reg.scratch; src = src2; size }
            ; Mov { dst = Reg RAX; src = src1; size }
            ; (* sign extend RAX into RDX:RAX *)
              Cqo
            ; Idiv { src = Reg Mach_reg.scratch; size }
              (* dst here isn't clobbered because in the register allocator we prevent dst from being allocated RAX or RDX *)
            ; Mov { dst; src = Reg RAX; size }
            ])
     | Imod ->
       Flat_x86.Instr.(
         empty
         +> [ Mov { dst = Reg Mach_reg.scratch; src = src2; size }
            ; Mov { dst = Reg RAX; src = src1; size }
            ; (* sign extend RAX into RDX:RAX *)
              Cqo
            ; Idiv { src = Reg Mach_reg.scratch; size }
              (* dst here isn't clobbered because in the register allocator we prevent dst from being allocated RAX or RDX *)
            ; Mov { dst; src = Reg RDX; size }
            ]))
  | Ret { src; size } ->
    let src = lower_operand st src in
    Flat_x86.Instr.(empty +> [ Mov { dst = Reg RAX; src; size } ] ++ epilogue)
;;

let lower_block st (block : Abs_x86.Block.t) : Flat_x86.Instr.t Bag.t =
  let instrs =
    block
    |> Abs_x86.Block.instrs
    |> Arrayp.to_list
    |> List.map ~f:(fun instr -> lower_instr st instr.i)
    |> Bag.concat
  in
  instrs
;;

let lower_func st (func : Abs_x86.Func.t) : Flat_x86.Program.t =
  let start_block = Ident.Map.find_exn func.blocks func.start in
  let instrs =
    empty
    +> Flat_x86.Instr.[ Directive ".text"; Directive ".globl _c0_main"; Label "_c0_main" ]
    ++ prologue 0l
    ++ lower_block st start_block
  in
  Bag.to_list instrs
;;

let lower allocation func =
  let st = create_state allocation in
  lower_func st func
;;
