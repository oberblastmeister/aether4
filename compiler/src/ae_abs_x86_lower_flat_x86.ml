open Std
module Abs_x86 = Ae_abs_x86_types
module Entity = Ae_entity_std
module Ident = Entity.Ident
module Flat_x86 = Ae_flat_x86_types
module Bag = Ae_data_bag
module Regalloc = Ae_abs_x86_regalloc
module Mach_reg = Ae_x86_mach_reg
module Label_entity = Ae_label_entity
module Label = Ae_label_entity.Ident
module Condition_code = Ae_x86_condition_code

let empty = Bag.empty

open Bag.Syntax

type st = { allocation : Regalloc.Allocation.t }

let create_state allocation = { allocation }
let get_vreg t vreg = Regalloc.Allocation.find_exn t.allocation vreg

let lower_operand st (operand : Abs_x86.Operand.t) : Flat_x86.Operand.t =
  match operand with
  | Imm i -> Imm i
  | Stack_slot _ -> todol [%here]
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

let label_to_string st (label : Label.t) =
  [%string ".L%{label.name}_%{label.id#Entity.Id}"]
;;

let lower_cmp (cc : Condition_code.t) size ~dst ~src1 ~src2 =
  empty +> Flat_x86.Instr.[ Cmp { src1; src2; size }; Set { dst; cc } ]
;;

let lower_simple_rmw rmw size ~dst ~src1 ~src2 =
  empty
  +> Flat_x86.Instr.
       [ Mov { dst = Reg Mach_reg.scratch; src = src1; size }
       ; rmw ~dst:(Flat_x86.Operand.Reg Mach_reg.scratch) ~src:src2 ~size
       ; Mov { dst; src = Reg Mach_reg.scratch; size }
       ]
;;

let lower_instr st (instr : Abs_x86.Instr.t) : Flat_x86.Instr.t Bag.t =
  match instr with
  | Block_params _ -> empty
  | Nop -> empty
  | Push { src } ->
    let src = lower_operand st src in
    empty +> Flat_x86.Instr.[ Push { src; size = Qword } ]
  | Pop { dst } ->
    let dst = lower_operand st dst in
    empty +> Flat_x86.Instr.[ Pop { dst; size = Qword } ]
  | Jump bc -> empty +> Flat_x86.Instr.[ Jmp (label_to_string st bc.label) ]
  | Cond_jump { cond; b1; b2 } ->
    let instrs =
      match cond with
      | Op src ->
        let src = lower_operand st src in
        (* only needs Byte, but we use Dword here *)
        empty
        +> Flat_x86.Instr.
             [ Mov { dst = Reg R11; src; size = Byte }
             ; Test { src1 = Reg R11; src2 = Reg R11; size = Byte }
             ; J { cc = NE; label = label_to_string st b1.label }
             ; Jmp (label_to_string st b2.label)
             ]
      | _ -> todo ()
    in
    instrs
  | Mov { src; dst; size } ->
    let src = lower_operand st src in
    let dst = lower_operand st dst in
    Flat_x86.Instr.(empty +> [ Mov { src; dst; size } ])
  | Mov_abs { dst; src } ->
    let dst = lower_operand st dst in
    Flat_x86.Instr.(empty +> [ Mov_abs { dst; src } ])
  | Bin { dst; op; src1; src2 } ->
    let dst = lower_operand st dst in
    let src1 = lower_operand st src1 in
    let src2 = lower_operand st src2 in
    let on_cmp ?(size = Flat_x86.Size.Qword) cc = lower_cmp cc size ~dst ~src1 ~src2 in
    let on_rmw ?(size = Flat_x86.Size.Qword) rmw =
      lower_simple_rmw rmw size ~dst ~src1 ~src2
    in
    let size = Flat_x86.Size.Qword in
    (match op with
     | Eq size -> on_cmp ~size E
     | Lshift ->
       empty
       +> Flat_x86.Instr.
            [ (* assembler will fail if src2 is more than 8 bit immediate on sal, so move it to rcx first *)
              Mov { dst = Reg R11; src = src1; size }
            ; Mov { dst = Reg RCX; src = src2; size = Dword }
            ; Sal { dst = Reg R11; size }
            ; Mov { dst; src = Reg R11; size }
            ]
     | Rshift ->
       empty
       +> Flat_x86.Instr.
            [ (* assembler will fail if src2 is more than 8 bit immediate on sal, so move it to rcx first *)
              Mov { dst = Reg R11; src = src1; size }
            ; Mov { dst = Reg RCX; src = src2; size = Dword }
            ; Sar { dst = Reg R11; size }
            ; Mov { dst; src = Reg R11; size }
            ]
     | Lt -> on_cmp L
     | Gt -> on_cmp G
     | Le -> on_cmp LE
     | Ge -> on_cmp GE
     | And size -> on_rmw ~size Flat_x86.Instr.and_
     | Or size -> on_rmw ~size Flat_x86.Instr.or_
     | Xor size -> on_rmw ~size Flat_x86.Instr.xor
     | Add -> on_rmw Flat_x86.Instr.add
     | Sub -> on_rmw Flat_x86.Instr.sub
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
  empty +> Flat_x86.Instr.[ Label (label_to_string st block.label) ] ++ instrs
;;

let lower_func st (func : Abs_x86.Func.t) : Flat_x86.Program.t =
  let labels =
    (* fold from left for reverse postorder *)
    Label_entity.Dfs.postorder ~start:[ func.start ] (Abs_x86.Func.graph func)
    |> Vec.fold ~init:[] ~f:(Fn.flip List.cons)
  in
  let instrs =
    List.map labels ~f:(fun label ->
      lower_block st (Ident.Map.find_exn func.blocks label))
    |> Bag.concat
  in
  let instrs =
    empty
    +> Flat_x86.Instr.[ Directive ".text"; Directive ".globl _c0_main"; Label "_c0_main" ]
    ++ prologue 0l
    ++ instrs
  in
  Bag.to_list instrs
;;

let lower allocation func =
  let st = create_state allocation in
  lower_func st func
;;
