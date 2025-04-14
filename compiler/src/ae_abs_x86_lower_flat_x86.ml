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
module Frame_layout = Ae_abs_x86_frame_layout
module Address = Ae_x86_address

let ins ?info i = Flat_x86.Line.Instr { i; info }
let empty = Bag.empty

open Bag.Syntax

type st =
  { frame_layout : Frame_layout.t
  ; allocation : Regalloc.Allocation.t
  }

let get_temp t (temp : Abs_x86.Temp.t) = Regalloc.Allocation.find_exn t.allocation temp

let lower_operand st (operand : Abs_x86.Operand.t) : Flat_x86.Operand.t =
  match operand with
  | Imm i -> Imm i
  | Stack_slot slot ->
    let offset = Frame_layout.resolve_frame_base_offset st.frame_layout slot in
    Mem (Address.create Mach_reg.RBP offset)
  | Reg temp -> Reg (get_temp st temp)
  | Mem _ -> todol [%here]
;;

let prologue ?info stack_size =
  let ins = ins ?info in
  empty
  +> [ ins (Push { src = Reg RBP; size = Qword })
     ; ins (Mov { dst = Reg RBP; src = Reg RSP; size = Qword })
     ; ins (Sub { dst = Reg RSP; src = Imm stack_size; size = Qword })
     ]
;;

let prologue_without_base ?info stack_size =
  let ins = ins ?info in
  empty +> [ ins (Sub { dst = Reg RSP; src = Imm stack_size; size = Qword }) ]
;;

let epilogue ?info () =
  let ins = ins ?info in
  empty
  +> [ ins (Mov { dst = Reg RSP; src = Reg RBP; size = Qword })
     ; ins (Pop { dst = Reg RBP; size = Qword })
     ; ins Ret
     ]
;;

let epilogue_without_base ?info stack_size =
  let ins = ins ?info in
  empty +> [ ins (Add { dst = Reg RSP; src = Imm stack_size; size = Qword }); ins Ret ]
;;

let label_to_string _st (label : Label.t) =
  [%string ".L%{label.name}_%{label.id#Entity.Id}"]
;;

let lower_cmp ?info (cc : Condition_code.t) size ~dst ~src1 ~src2 =
  let ins = ins ?info in
  empty +> [ ins (Cmp { src1; src2; size }); ins (Set { dst; cc }) ]
;;

let lower_simple_rmw ?info rmw size ~dst ~src1 ~src2 =
  let ins = ins ?info in
  empty
  +> [ ins (Mov { dst = Reg Mach_reg.scratch; src = src1; size })
     ; ins (rmw ~dst:(Flat_x86.Operand.Reg Mach_reg.scratch) ~src:src2 ~size)
     ; ins (Mov { dst; src = Reg Mach_reg.scratch; size })
     ]
;;

let lower_instr st (instr : Abs_x86.Instr'.t) : Flat_x86.Line.t Bag.t =
  let ins = ins ?info:instr.info in
  match instr.i with
  | Unreachable ->
    (* TODO: lower this to a panic in debug mode *)
    empty
  | Block_params _ -> empty
  | Nop -> empty
  | Jump bc -> empty +> [ ins (Jmp (label_to_string st bc.label)) ]
  | Cond_jump { cond; b1; b2 } ->
    let instrs =
      match cond with
      | Op src ->
        let src = lower_operand st src in
        (* only needs Byte, but we use Dword here *)
        empty
        +> Flat_x86.Instr.
             [ ins (Mov { dst = Reg R11; src; size = Dword })
               (* Important! This must be Byte because only the lower byte is valid for byte size, even though we use Dword registers *)
             ; ins (Test { src1 = Reg R11; src2 = Reg R11; size = Byte })
             ; ins (J { cc = NE; label = label_to_string st b1.label })
             ; ins (Jmp (label_to_string st b2.label))
             ]
      | _ -> todo ()
    in
    instrs
  (*
     TODO: this is very wrong.
  *)
  | Mov { src; dst; size } ->
    let src = lower_operand st src in
    let dst = lower_operand st dst in
    empty +> [ ins (Mov { dst; src; size }) ]
    (* (
      empty
      +> [ ins (Mov { dst = Reg R11; src; size })
         ; ins (Mov { dst; src = Reg R11; size })
         ]) *)
  | Mov_abs { dst; src } ->
    let dst = lower_operand st dst in
    Flat_x86.Instr.(empty +> [ ins (Mov_abs { dst; src }) ])
  | Bin { dst; op; src1; src2 } ->
    let dst = lower_operand st dst in
    let src1 = lower_operand st src1 in
    let src2 = lower_operand st src2 in
    let on_cmp ?(size = Flat_x86.Ty.Qword) cc =
      lower_cmp ?info:instr.info cc size ~dst ~src1 ~src2
    in
    let on_rmw ?(size = Flat_x86.Ty.Qword) rmw =
      lower_simple_rmw ?info:instr.info rmw size ~dst ~src1 ~src2
    in
    let size = Flat_x86.Ty.Qword in
    (match op with
     | Eq size -> on_cmp ~size E
     | Lshift ->
       empty
       +> [ (* assembler will fail if src2 is more than 8 bit immediate on sal, so move it to rcx first *)
            ins (Mov { dst = Reg R11; src = src1; size })
          ; ins (Mov { dst = Reg RCX; src = src2; size = Dword })
          ; ins (Sal { dst = Reg R11; size })
          ; ins (Mov { dst; src = Reg R11; size })
          ]
     | Rshift ->
       empty
       +> Flat_x86.Instr.
            [ (* assembler will fail if src2 is more than 8 bit immediate on sal, so move it to rcx first *)
              ins (Mov { dst = Reg R11; src = src1; size })
            ; ins (Mov { dst = Reg RCX; src = src2; size = Dword })
            ; ins (Sar { dst = Reg R11; size })
            ; ins (Mov { dst; src = Reg R11; size })
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
              ins (Mov { dst = Reg Mach_reg.scratch; src = src1; size })
            ; ins (Mov { dst = Reg RAX; src = src2; size })
            ; ins (Imul { src = Reg Mach_reg.scratch; size })
              (* dst here isn't clobbered because in the register allocator we prevent dst from being allocated RAX or RDX *)
            ; ins (Mov { dst; src = Reg RAX; size })
            ])
     | Idiv ->
       Flat_x86.Instr.(
         empty
         +> [ ins (Mov { dst = Reg Mach_reg.scratch; src = src2; size })
            ; ins (Mov { dst = Reg RAX; src = src1; size })
            ; (* sign extend RAX into RDX:RAX *)
              ins Cqo
            ; ins (Idiv { src = Reg Mach_reg.scratch; size })
              (* dst here isn't clobbered because in the register allocator we prevent dst from being allocated RAX or RDX *)
            ; ins (Mov { dst; src = Reg RAX; size })
            ])
     | Imod ->
       Flat_x86.Instr.(
         empty
         +> [ ins (Mov { dst = Reg Mach_reg.scratch; src = src2; size })
            ; ins (Mov { dst = Reg RAX; src = src1; size })
            ; (* sign extend RAX into RDX:RAX *)
              ins Cqo
            ; ins (Idiv { src = Reg Mach_reg.scratch; size })
              (* dst here isn't clobbered because in the register allocator we prevent dst from being allocated RAX or RDX *)
            ; ins (Mov { dst; src = Reg RDX; size })
            ]))
  | Ret { src; size } ->
    let src = lower_operand st src in
    Flat_x86.Instr.(
      empty
      +> [ ins (Mov { dst = Reg RAX; src; size }) ]
      ++ epilogue ?info:(Option.map instr.info ~f:(Info.tag ~tag:"epilogue")) ())
  | Call _ -> todol [%here]
;;

let lower_block st (block : Abs_x86.Block.t) : Flat_x86.Line.t Bag.t =
  let instrs =
    block
    |> Abs_x86.Block.instrs
    |> Arrayp.to_list
    |> List.map ~f:(lower_instr st)
    |> Bag.concat
  in
  empty +> Flat_x86.Line.[ Label (label_to_string st block.label) ] ++ instrs
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
    +> Flat_x86.Line.[ Directive ".text"; Directive ".globl _c0_main"; Label "_c0_main" ]
    ++ prologue
         ~info:(Info.create_s [%message "prologue"])
         (Int32.of_int_exn (Frame_layout.frame_size st.frame_layout))
    ++ instrs
  in
  Bag.to_list instrs
;;

let lower ~frame_layout ~allocation func =
  let st = { frame_layout; allocation } in
  lower_func st func
;;
