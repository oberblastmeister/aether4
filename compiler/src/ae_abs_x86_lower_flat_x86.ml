(*
   TODO: possibly promote byte moves to double word moves
  prefer 32 bit registers: https://stackoverflow.com/questions/41573502/why-doesnt-gcc-use-partial-registers
*)
open Std
module Abs_x86 = Ae_abs_x86_types
module Flat_x86 = Ae_flat_x86_types
module Bag = Ae_data_bag
module Regalloc = Ae_abs_x86_regalloc
module Mach_reg = Ae_x86_mach_reg
module Label = Ae_label
module Condition_code = Ae_x86_condition_code
module Frame_layout = Ae_abs_x86_frame_layout
module Address = Ae_x86_address
module Call_conv = Ae_call_conv
module X86_call_conv = Ae_x86_call_conv
module Temp = Ae_temp

let ins ?info i = Flat_x86.Line.Instr { i; info }
let empty = Bag.empty

open Bag.Syntax

type st =
  { frame_layout : Frame_layout.t
  ; allocation : int Abs_x86.Temp.Table.t
  ; func_index : int
  }

let get_temp t (temp : Abs_x86.Temp.t) = Mach_reg.of_enum_exn t.allocation.Temp.!(temp)

let lower_operand st (operand : Abs_x86.Operand.t) : Flat_x86.Operand.t =
  match operand with
  | Imm i -> Imm i
  | Stack stack_addr -> begin
    match stack_addr with
    | Slot slot ->
      let offset = Frame_layout.resolve_frame_base_offset st.frame_layout slot in
      Mem (Address.create Mach_reg.RBP offset)
    | Previous_frame i ->
      (* -16 to skip over the pushed return address and pushed base pointer *)
      Mem (Address.create Mach_reg.RBP (16 + Int.of_int32_exn i))
    | Current_frame i -> Mem (Address.create Mach_reg.RSP (Int.of_int32_exn i))
  end
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

let label_to_string (st : st) (label : Label.t) =
  [%string ".L%{label.name}_%{st.func_index#Int}_%{label.id#Int}"]
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

let lower_mov ?info st ~dst ~src ~size =
  let ins = ins ?info in
  let src = lower_operand st src in
  let dst = lower_operand st dst in
  if Flat_x86.Operand.equal dst src
  then empty
  else if Flat_x86.Operand.is_mem dst && Flat_x86.Operand.is_mem src
  then
    (* memory to memory move, break with scratch register *)
    empty
    +> [ ins (Mov { dst = Reg R11; src; size }); ins (Mov { dst; src = Reg R11; size }) ]
  else empty +> [ ins (Mov { dst; src; size }) ]
;;

let lower_instr st (instr : Abs_x86.Instr'.t) : Flat_x86.Line.t Bag.t =
  let ins = ins ?info:instr.info in
  match instr.i with
  | Unreachable ->
    (* TODO: lower this to a panic in debug mode *)
    empty
  | Undefined { dst; size } ->
    let dst = lower_operand st dst in
    (* just for safety, remove this in performance mode *)
    empty +> [ ins (Mov { dst; src = Imm 0xAAAAAAAAl; size }) ]
  | Block_params _ -> empty
  | Push { src; size } ->
    let src = get_temp st src in
    empty +> [ ins (Push { src = Reg src; size }) ]
  | Pop { dst; size } ->
    let dst = get_temp st dst in
    empty +> [ ins (Pop { dst = Reg dst; size }) ]
  | Nop -> empty
  | Jump bc -> empty +> [ ins (Jmp (label_to_string st bc.label)) ]
  | Cond_jump { cond; b1; b2 } ->
    let instrs =
      match cond with
      | Op src ->
        let src = lower_operand st src in
        (* only needs Byte, but we use Dword here *)
        empty
        +> [ ins (Mov { dst = Reg R11; src; size = Dword })
             (* Important! This must be Byte because only the lower byte is valid for byte size, even though we use Dword registers *)
           ; ins (Test { src1 = Reg R11; src2 = Reg R11; size = Byte })
           ; ins (J { cc = NE; label = label_to_string st b1.label })
           ; ins (Jmp (label_to_string st b2.label))
           ]
      | _ -> todo ()
    in
    instrs
  (*
     TODO: account for memory to memory moves
  *)
  | Mov { src; dst; size } -> lower_mov st ?info:instr.info ~dst ~src ~size
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
    begin
      match op with
      | Eq size -> on_cmp ~size E
      | Lshift ->
        assert (Flat_x86.Operand.reg_val src2 |> Option.value_exn |> Mach_reg.equal RCX);
        empty
        +> [ (* assembler will fail if src2 is more than 8 bit immediate on sal, so move it to rcx first *)
             ins (Mov { dst = Reg R11; src = src1; size })
           ; ins (Mov { dst = Reg RCX; src = src2; size = Dword })
           ; ins (Sal { dst = Reg R11; size })
           ; ins (Mov { dst; src = Reg R11; size })
           ]
      | Rshift ->
        assert (Flat_x86.Operand.reg_val src2 |> Option.value_exn |> Mach_reg.equal RCX);
        empty
        +> [ (* assembler will fail if src2 is more than 8 bit immediate on sal, so move it to rcx first *)
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
      | Imul -> on_rmw Flat_x86.Instr.imul
      | Idiv ->
        assert (Flat_x86.Operand.reg_val src1 |> Option.value_exn |> Mach_reg.equal RAX);
        assert (Flat_x86.Operand.reg_val dst |> Option.value_exn |> Mach_reg.equal RAX);
        empty
        +> [ ins (Mov { dst = Reg Mach_reg.scratch; src = src2; size })
           ; ins (Mov { dst = Reg RAX; src = src1; size })
           ; (* sign extend RAX into RDX:RAX *)
             ins Cqo
           ; ins (Idiv { src = Reg Mach_reg.scratch; size })
             (* dst here isn't clobbered because in the register allocator we prevent dst from being allocated RAX or RDX *)
           ; ins (Mov { dst; src = Reg RAX; size })
           ]
      | Imod ->
        assert (Flat_x86.Operand.reg_val src1 |> Option.value_exn |> Mach_reg.equal RAX);
        assert (Flat_x86.Operand.reg_val dst |> Option.value_exn |> Mach_reg.equal RAX);
        empty
        +> [ ins (Mov { dst = Reg Mach_reg.scratch; src = src2; size })
           ; ins (Mov { dst = Reg RAX; src = src1; size })
           ; (* sign extend RAX into RDX:RAX *)
             ins Cqo
           ; ins (Idiv { src = Reg Mach_reg.scratch; size })
             (* dst here isn't clobbered because in the register allocator we prevent dst from being allocated RAX or RDX *)
           ; ins (Mov { dst; src = Reg RDX; size })
           ]
    end
  | Ret { src; size } ->
    let src = lower_operand st src in
    Flat_x86.Instr.(
      empty
      +> [ ins (Mov { dst = Reg RAX; src; size }) ]
      ++ epilogue ?info:(Option.map instr.info ~f:(Info.tag ~tag:"epilogue")) ())
  | Call { dst; size = _; func; args; call_conv } ->
    let args_in_registers, args_rem = List.zip_with_remainder args call_conv.call_args in
    let reg_moves =
      List.map args_in_registers ~f:(fun ((arg, ty), reg_expected) ->
        match arg with
        | Temp temp ->
          let reg = get_temp st temp in
          assert (Mach_reg.equal reg reg_expected);
          empty
        | _ ->
          empty
          +> [ ins
                 (Mov
                    { dst = Reg reg_expected
                    ; src = lower_operand st (Abs_x86.Location.to_operand arg)
                    ; size = ty
                    })
             ])
      |> Bag.concat
    in
    assert (Mach_reg.equal (get_temp st dst) call_conv.return_reg);
    let stack_moves =
      match args_rem with
      | Some (First args_on_stack) ->
        List.mapi args_on_stack ~f:(fun i (loc, ty) ->
          lower_mov
            st
            ?info:instr.info
            ~dst:(Stack (Current_frame (Int32.of_int_exn (i * 8))))
            ~src:(Abs_x86.Location.to_operand loc)
            ~size:ty)
        |> Bag.concat
      | _ -> Bag.empty
    in
    empty ++ stack_moves ++ reg_moves +> [ ins (Call func) ]
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
    Label.Dfs.postorder ~start:[ func.start ] (Abs_x86.Func.graph func)
    |> Vec.fold ~init:[] ~f:(Fn.flip List.cons)
  in
  let instrs =
    List.map labels ~f:(fun label ->
      lower_block st (Abs_x86.Func.find_block_exn func label))
    |> Bag.concat
  in
  let instrs =
    empty
    +> Flat_x86.Line.[ Directive (".globl " ^ func.name); Label func.name ]
    ++ prologue
         ~info:(Info.create_s [%message "prologue"])
         (Int32.of_int_exn (Frame_layout.frame_size st.frame_layout))
    ++ instrs
  in
  Bag.to_list instrs
;;

let push_callee_saved =
  X86_call_conv.callee_saved
  |> List.map ~f:(fun reg ->
    Flat_x86.Line.Instr
      { i = Flat_x86.Instr.Push { src = Reg reg; size = Qword }; info = None })
;;

let pop_callee_saved =
  X86_call_conv.callee_saved
  |> List.rev
  |> List.map ~f:(fun reg ->
    Flat_x86.Line.Instr
      { i = Flat_x86.Instr.Pop { dst = Reg reg; size = Qword }; info = None })
;;

let c0_main_export_instructions =
  Flat_x86.Line.[ Directive ".globl c0_main_export"; Label "c0_main_export" ]
  @ push_callee_saved
  @ [ Flat_x86.Line.Instr { i = Call "_c0_main"; info = None } ]
  @ pop_callee_saved
  @ [ Flat_x86.Line.Instr { i = Ret; info = None } ]
;;

let lower ~frame_layout ~allocation ~func_index func =
  let st = { frame_layout; allocation; func_index } in
  lower_func st func
;;
