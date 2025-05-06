open Std
module Tir = Ae_tir_types
module Label = Ae_label
module Temp = Ae_temp
module Bag = Ae_data_bag
module Lir = Ae_lir_types
module X86_call_conv = Ae_x86_call_conv
open Bag.Syntax
open Ae_trace

module Api = struct
  let empty = Bag.empty
  let ins ?ann ?info i = Second (Lir.Instr'.create_unindexed ?ann ?info i)
  let label l = empty +> [ First l; ins (Block_params []) ]
  let bc ?(args = []) label = { Lir.Block_call.label; args }

  let const_i1 ?ann ?info dst const =
    ins ?ann ?info (Nullary { dst; op = Int_const { const; ty = I1 } })
  ;;

  let const_i64 ?ann ?info dst const =
    ins ?ann ?info (Nullary { dst; op = Int_const { const; ty = I64 } })
  ;;

  open struct
    let bin (op : Lir.Bin_op.t) ?ann ?info dst src1 src2 =
      ins ?ann ?info (Bin { dst; op; src1; src2 })
    ;;

    let unary (op : Lir.Unary_op.t) ?ann ?info dst src =
      ins ?ann ?info (Unary { dst; op; src })
    ;;
  end

  let undefined ty ?ann ?info dst = ins ?ann ?info (Nullary { dst; op = Undefined ty })
  let undefined_i64 = undefined I64
  let add = bin Add
  let mul = bin Mul
  let sub = bin Sub
  let div = bin Div
  let le = bin Le
  let copy_i64 = unary (Copy I64)
  let load_i64 = unary (Load I64)
  let store_i64 = bin (Store I64)
  let call ~dsts ~call_conv func args = ins (Call { dsts; func; args; call_conv })
  let eq_i64 = bin (Eq I64)
  let eq_i1 = bin (Eq I1)
  let unreachable = ins Unreachable
end

open Api

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
  | Ptr -> I64
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
  | Offset_ptr -> Add
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

let lower_align st align dst src =
  let align_temp = fresh_temp ~name:"align" st in
  let align_temp' = fresh_temp ~name:"align'" st in
  empty
  +> [ const_i64 align_temp (Int64.of_int_exn align)
     ; const_i64 align_temp' (Int64.of_int_exn (align - 1))
     ; add dst src align_temp'
     ; div dst dst align_temp
     ; mul dst dst align_temp
     ]
;;

let lower_check_hp st new_hp =
  let hp_lim = fresh_temp ~name:"HP_LIM" st in
  let hp_cmp = fresh_temp ~name:"hp_cmp" st in
  empty
  +> [ load_i64 hp_lim st.cx_temp; le hp_cmp new_hp hp_lim ]
  ++ make_cond
       st
       hp_cmp
       empty
       (empty
        +> [ call ~dsts:[] ~call_conv:X86_call_conv.sysv "c0_runtime_alloc_fail" []
           ; unreachable
           ])
;;

let lower_alloc st dst ~size ~align =
  let new_hp = fresh_temp ~name:"HP'" st in
  empty
  ++ lower_align st align dst st.hp_temp
  +> [ add new_hp dst size ]
  ++ lower_check_hp st new_hp
  +> [ copy_i64 st.hp_temp new_hp ]
;;

let lower_instr st ~is_start_block (instr : Tir.Instr'.t) : instrs =
  let ins = ins ?info:instr.info in
  match instr.i with
  | Nop -> empty
  | Call { dst; ty; func; args; is_extern } ->
    lower_call st ~dsts:[ dst, ty ] ~func ~args ~is_extern
  | Unreachable -> empty +> [ unreachable ]
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
    | Null_ptr -> empty +> [ const_i64 dst 0L ]
    | Int_const const -> empty +> [ const_i64 dst const ]
    | Bool_const const ->
      let const =
        match const with
        | true -> 1L
        | false -> 0L
      in
      empty +> [ const_i1 dst const ]
    | Void_const -> empty +> [ undefined_i64 dst ]
    | Alloc { size; align } ->
      assert (Int.is_pow2 align);
      let const_temp = fresh_temp ~name:"const" st in
      empty
      +> [ const_i64 const_temp (Int64.of_int_exn size) ]
      ++ lower_alloc st dst ~size:const_temp ~align
  end
  | Unary { dst; op; src } -> begin
    match op with
    | Copy ty -> empty +> [ ins (Unary { dst; op = Copy (lower_ty ty); src }) ]
    | Deref ty ->
      let ty = lower_ty ty in
      empty +> [ ins (Unary { dst; op = Load ty; src }) ]
    | Alloc_array { size = elem_size; align } ->
      assert (Int.is_pow2 align);
      let elem_size_temp = fresh_temp ~name:"const" st in
      let size_temp = fresh_temp ~name:"size" st in
      let word_temp = fresh_temp ~name:"word" st in
      let garbage_temp = fresh_temp ~name:"garbage" st in
      empty
      +> [ const_i64 elem_size_temp (Int64.of_int_exn elem_size)
         ; mul size_temp src elem_size_temp
         ; const_i64 word_temp 8L
         ; add size_temp size_temp word_temp
         ]
      ++ lower_alloc st dst ~size:size_temp ~align:(max align 8)
      +> [ store_i64 garbage_temp dst src ]
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
    | C0_runtime_null_pointer_panic, [] ->
      lower_call
        st
        ~dsts:[ dst, Void ]
        ~func:"c0_runtime_null_pointer_panic"
        ~args:[]
        ~is_extern:true
    | (C0_runtime_assert | C0_runtime_null_pointer_panic), _ ->
      raise_s [%message "Invalid operation configuration"]
  end
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
