open Std
module Tir = Ae_tir_types
module Label = Ae_label
module Temp = Ae_temp
module Bag = Ae_data_bag
module Lir = Ae_lir_types
module X86_call_conv = Ae_x86_call_conv
open Bag.Syntax
open Ae_trace

let empty = Bag.empty
let ins ?ann ?info i = Second (Lir.Instr'.create_unindexed ?ann ?info i)
let label l = empty +> [ First l; ins (Block_params []) ]
let bc ?(args = []) label = { Lir.Block_call.label; args }

type st =
  { temp_gen : Temp.Id_gen.t
  ; label_gen : Label.Id_gen.t
  ; cx_temp : Temp.t
  ; hp_temp : Temp.t
  }

type instrs = Lir.Linearized.instr Bag.t

let create_state func =
  let temp_gen = Tir.Func.create_temp_gen func in
  let label_gen = Tir.Func.create_label_gen func in
  let hp_temp = Temp.fresh ~name:"HP" temp_gen in
  let cx_temp = Temp.fresh ~name:"CX" temp_gen in
  { temp_gen; label_gen; cx_temp; hp_temp }
;;

let fresh_temp ?(name = "fresh") ?info t : Lir.Temp.t =
  let id = Temp.Id_gen.get t.temp_gen in
  Temp.create ?info name id
;;

let fresh_label ?(name = "fresh") ?info t : Label.t =
  let id = Label.Id_gen.get t.label_gen in
  Label.create ?info name id
;;

let make_cond st cond body1 body2 =
  let then_label = fresh_label ~name:"then" st in
  let else_label = fresh_label ~name:"else" st in
  let join_label = fresh_label ~name:"join" st in
  empty
  +> [ ins (Cond_jump { cond; b1 = bc then_label; b2 = bc else_label }) ]
  ++ label then_label
  ++ body1
  +> [ ins (Jump (bc join_label)) ]
  ++ label else_label
  ++ body2
  +> [ ins (Jump (bc join_label)) ]
  ++ label join_label
;;

let lower_ty (ty : Tir.Ty.t) : Lir.Ty.t =
  match ty with
  | Int -> I64
  | Bool -> I1
  | Void -> I64
  | Pointer _ -> I64
;;

let lower_bin_op (op : Tir.Bin_op.t) : Lir.Bin_op.t =
  match op with
  | Add -> Add
  | Sub -> Sub
  | Mul -> Mul
  | Div -> Div
  | Mod -> Mod
  | Lt -> Lt
  | Gt -> Gt
  | Le -> Le
  | Ge -> Ge
  | And -> And I64
  | Or -> Or I64
  | Xor -> Xor I64
  | Eq ty -> Eq (lower_ty ty)
  | Lshift -> Lshift
  | Rshift -> Rshift
  | Store ty -> Store (lower_ty ty)
;;

let lower_block_call (b : Tir.Block_call.t) : Lir.Block_call.t =
  { label = b.label; args = b.args }
;;

let mangle_func_name name = "_c0_" ^ name

let lower_call st ~dsts ~func ~args ~is_extern =
  let dsts = (if is_extern then [] else [ st.hp_temp, Tir.Ty.Int ]) @ dsts in
  let args =
    (if is_extern then [] else [ st.cx_temp, Tir.Ty.Int; st.hp_temp, Tir.Ty.Int ]) @ args
  in
  let dsts = (List.map & Tuple2.map_snd) dsts ~f:lower_ty in
  let func = if is_extern then func else mangle_func_name func in
  let args = (List.map & Tuple2.map_snd) args ~f:lower_ty in
  let call_conv = if is_extern then X86_call_conv.sysv else X86_call_conv.c0 in
  empty +> [ ins (Call { dsts; func; args; call_conv }) ]
;;

let lower_instr st ~is_start_block (instr : Tir.Instr'.t) : instrs =
  let ins = ins ?info:instr.info in
  match instr.i with
  | Nop -> empty
  | Call { dst; ty; func; args; is_extern } ->
    lower_call st ~dsts:[ dst, ty ] ~func ~args ~is_extern
  | Unreachable -> empty +> [ ins Unreachable ]
  | Jump b ->
    let b = lower_block_call b in
    empty +> [ ins (Jump b) ]
  | Cond_jump { cond; b1; b2 } ->
    let b1 = lower_block_call b1 in
    let b2 = lower_block_call b2 in
    empty +> [ ins (Cond_jump { cond; b1; b2 }) ]
  | Block_params temps ->
    let temps =
      (if is_start_block
       then
         [ { Tir.Block_param.param = st.cx_temp; ty = Int }
         ; { param = st.hp_temp; ty = Int }
         ]
       else [])
      @ temps
    in
    empty
    +> [ ins
           (Lir.Instr.Block_params
              (List.map
                 ~f:(fun { param; ty } ->
                   let ty = lower_ty ty in
                   { Lir.Block_param.param; ty })
                 temps))
       ]
  | Nullary { dst; op } -> begin
    match op with
    | Int_const const ->
      empty +> [ ins (Nullary { dst; op = Int_const { const; ty = I64 } }) ]
    | Bool_const const ->
      let const =
        match const with
        | true -> 1L
        | false -> 0L
      in
      empty +> [ ins (Nullary { dst; op = Int_const { const; ty = I1 } }) ]
    | Void_const -> empty +> [ ins (Nullary { dst; op = Undefined I64 }) ]
    | Alloc ty ->
      let size = Tir.Ty.get_byte_size ty in
      let size_temp = fresh_temp ~name:"size" st in
      let alloc_succeed_label = fresh_label ~name:"alloc_suceeed" st in
      let alloc_fail_label = fresh_label ~name:"alloc_fail" st in
      let alloc_join_label = fresh_label ~name:"alloc_join_label" st in
      let hp_lim = fresh_temp ~name:"HP_LIM" st in
      let hp_cmp = fresh_temp ~name:"hp_cmp" st in
      let new_hp = fresh_temp ~name:"new_hp" st in
      empty
      +> [ ins
             (Nullary
                { dst = size_temp
                ; op = Int_const { const = Int64.of_int size; ty = I64 }
                })
         ; ins (Bin { dst = new_hp; op = Add; src1 = st.hp_temp; src2 = size_temp })
         ; ins (Unary { dst = hp_lim; op = Load I64; src = st.cx_temp })
         ; ins (Bin { dst = hp_cmp; op = Le; src1 = new_hp; src2 = hp_lim })
         ; ins
             (Cond_jump
                { cond = hp_cmp
                ; b1 = { label = alloc_succeed_label; args = [] }
                ; b2 = { label = alloc_fail_label; args = [] }
                })
         ]
      ++ label alloc_succeed_label
      +> [ (* set the result temporary *)
           ins (Unary { dst; op = Copy I64; src = st.hp_temp })
         ; ins (Unary { dst = st.hp_temp; op = Copy I64; src = new_hp })
         ; ins (Jump { label = alloc_join_label; args = [] })
         ]
      ++ label alloc_fail_label
      +>
      (* we have to define dst so it is in strict ssa form *)
      [ ins (Nullary { dst; op = Undefined I64 })
      ; ins
          (Call
             { dsts = []
             ; func = "c0_runtime_alloc_fail"
             ; args = []
             ; call_conv = X86_call_conv.sysv
             })
      ; ins Unreachable
      ]
      ++ label alloc_join_label
  end
  | Unary { dst; op; src } -> begin
    match op with
    | Copy ty -> empty +> [ ins (Unary { dst; op = Copy (lower_ty ty); src }) ]
    | Deref ty ->
      let ty = lower_ty ty in
      let garbage_temp = fresh_temp ~name:"garbage_temp" st in
      let body1 =
        empty
        +> [ ins
               (Call
                  { dsts = [ garbage_temp, I64 ]
                  ; func = "c0_runtime_deref_fail"
                  ; args = []
                  ; call_conv = X86_call_conv.sysv
                  })
           ; ins (Nullary { dst; op = Undefined I64 })
           ; ins Unreachable
           ]
      in
      let body2 = empty +> [ ins (Unary { dst; op = Load ty; src }) ] in
      let cond_dst = fresh_temp ~name:"is_null_cond" st in
      let null_temp = fresh_temp ~name:"null" st in
      empty
      +> [ ins (Nullary { dst = null_temp; op = Int_const { const = 0L; ty = I64 } })
         ; ins (Bin { dst = cond_dst; op = Eq I64; src1 = src; src2 = null_temp })
         ]
      ++ make_cond st cond_dst body1 body2
  end
  | Bin { dst; op; src1; src2 } ->
    let op = lower_bin_op op in
    let instr = ins (Bin { dst; op; src1; src2 }) in
    empty +> [ instr ]
  | Nary { dst; op; srcs } -> begin
    match op, srcs with
    | C0_runtime_assert, [ t0; t1; t2; t3; t4 ] ->
      lower_call
        st
        ~dsts:[ dst, Void ]
        ~func:"c0_runtime_assert"
        ~args:[ t0, Bool; t1, Int; t2, Int; t3, Int; t4, Int ]
        ~is_extern:true
    | C0_runtime_assert, _ -> raise_s [%message "Invalid operation configuration"]
  end
  | Getelementptr _ -> todol [%here]
  | Ret { src; ty } ->
    let ty = lower_ty ty in
    empty
    +> [ ins (Ret { srcs = [ st.hp_temp, I64; src, ty ]; call_conv = X86_call_conv.c0 }) ]
;;

let lower_block st ~is_start_block (block : Tir.Block.t) : instrs =
  let instrs =
    Arrayp.to_list (Tir.Block.instrs block)
    |> List.map ~f:(lower_instr st ~is_start_block)
    |> Bag.concat
  in
  empty +> [ First block.label ] ++ instrs
;;

let lower_func (func : Tir.Func.t) : Lir.Func.t =
  let st = create_state func in
  let name = mangle_func_name func.name in
  let linearized =
    func.blocks
    |> Map.to_alist
    |> List.map ~f:snd
    |> List.map ~f:(fun block ->
      lower_block st ~is_start_block:(Label.equal block.label func.start) block)
    |> Bag.concat
    |> Bag.to_list
  in
  let blocks = Lir.Linearized.to_blocks_exn linearized in
  let start = func.start in
  let next_temp_id = Temp.Id_gen.get st.temp_gen in
  let next_label_id = Label.Id_gen.get st.label_gen in
  let call_conv = X86_call_conv.c0 in
  let func = { Lir.Func.name; blocks; start; next_temp_id; next_label_id; call_conv } in
  let func = Lir.Convert_ssa.convert func in
  Lir.Check.check func |> Or_error.ok_exn;
  func
;;

let lower_program (program : Tir.Program.t) : Lir.Program.t =
  let funcs = List.map program.funcs ~f:lower_func in
  { funcs }
;;
