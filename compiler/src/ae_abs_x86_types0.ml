open Std

open struct
  module Generic_ir = Ae_generic_ir_std
  module Call_conv = Ae_call_conv
end

module Ty = Ae_x86_ty
module Temp = Ae_temp
module Label = Ae_label
module Mach_reg = Ae_x86_mach_reg
module Stack_slot = Ae_stack_slot
module Address = Ae_x86_address.Make (Temp)

module Stack_address = struct
  type t =
    | Slot of Stack_slot.t
    | Previous_frame of int32
    | Current_frame of int32
  [@@deriving sexp_of, variants, equal]
end

module Operand = struct
  type t =
    | Imm of Int32.t
    | Reg of Temp.t
    | Mem of Address.t
    | Stack of Stack_address.t
  [@@deriving sexp_of, variants]

  let iter_mem_uses (o : t) ~f =
    match o with
    | Mem addr -> Address.iter_uses addr ~f
    | Reg _ | Stack _ | Imm _ -> ()
  ;;

  let iter_any_regs (o : t) ~f =
    match o with
    | Reg r ->
      f r;
      ()
    | Stack _ | Imm _ -> ()
    | Mem address -> Address.iter_uses address ~f
  ;;

  let iter_reg_val (o : t) ~f =
    match o with
    | Reg r ->
      f r;
      ()
    | Stack _ | Imm _ | Mem _ -> ()
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
    | Stack of Stack_address.t
  [@@deriving sexp_of, variants, equal]

  let to_either t =
    match t with
    | Temp temp -> Either.First temp
    | Stack slot -> Either.Second slot
  ;;

  let temp_val = function
    | Temp temp -> Some temp
    | Stack _ -> None
  ;;

  let iter_temp t ~f =
    match t with
    | Temp temp -> f temp
    | Stack _ -> ()
  ;;

  let map_temp t ~f =
    match t with
    | Temp temp -> Temp (f temp)
    | Stack _ -> t
  ;;

  let to_operand = function
    | Temp v -> Operand.Reg v
    | Stack s -> Operand.Stack s
  ;;

  let of_temp t = Temp t

  let equal_allocated allocation t1 t2 =
    match t1, t2 with
    | Temp t1, Temp t2 -> allocation.Temp.!(t1) = allocation.Temp.!(t2)
    | Stack s1, Stack s2 -> Stack_address.equal s1 s2
    | _ -> false
  ;;
end

module Block_call = struct
  type t =
    { label : Label.t
    ; args : Location.t list
    }
  [@@deriving sexp_of, fields ~fields ~getters ~iterators:create]

  let map_args t ~f = { label = t.label; args = f t.args }
end

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
    | Undefined of
        { dst : Operand.t
        ; size : Ty.t
        }
    | Push of
        { src : Temp.t
        ; size : Ty.t
        }
    | Pop of
        { dst : Temp.t
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
        { srcs : (Location.t * Ty.t) list
        ; call_conv : Call_conv.t
        }
    (*
       TODO: we should have an invariant that it can only have num_args_in_registers amount of args.
      The other args should be spilled by the instruction selector
    *)
    | Call of
        { dsts : (Location.t * Ty.t) list
        ; func : string
        ; args : (Location.t * Ty.t) list
          (*
             Important that we don't access call_conv.clobbers directly.
            Make sure to call iter_clobbers instead
          *)
        ; call_conv : Call_conv.t
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
    | Push { src; size = _ } ->
      on_use (Operand.Reg src);
      ()
    | Pop { dst; size } ->
      on_def (Operand.Reg dst, size);
      ()
    | Undefined { dst; size } -> on_def (dst, size)
    | Call { dsts; func = _; args; call_conv = _ } ->
      List.iter dsts ~f:(fun (loc, ty) ->
        Location.iter_temp loc ~f:(fun temp -> on_def (Operand.Reg temp, ty)));
      (List.iter @> Fold.of_fn fst @> Location.iter_temp) args ~f:(fun temp ->
        on_use (Operand.Reg temp))
    | Block_params params ->
      List.iter params ~f:(fun param ->
        Location.iter_temp param.param ~f:(fun temp ->
          on_def (Operand.Reg temp, param.ty)))
    | Jump b ->
      (List.iter @> Location.iter_temp) b.args ~f:(fun r -> on_use (Operand.Reg r));
      ()
    | Cond_jump { cond; b1; b2 } ->
      (match cond with
       | Op o -> on_use o
       | Bin { src1; op = _; src2 } ->
         on_use src1;
         on_use src2);
      (List.iter @> Location.iter_temp) b1.args ~f:(fun r -> on_use (Reg r));
      (List.iter @> Location.iter_temp) b2.args ~f:(fun r -> on_use (Reg r));
      ()
    | Mov { dst; src; size } ->
      on_use src;
      on_def (dst, size);
      ()
    | Mov_abs { dst; src = _ } ->
      on_def (dst, Qword);
      ()
    | Bin { dst; src1; op; src2 } ->
      on_use src1;
      on_use src2;
      let ty : Ty.t =
        match op with
        | And ty | Or ty | Xor ty -> ty
        | Add | Sub | Imul | Idiv | Imod | Lshift | Rshift -> Qword
        | Eq _ | Lt | Gt | Le | Ge -> Byte
      in
      on_def (dst, ty);
      ()
    | Ret { srcs; call_conv = _ } ->
      (List.iter @> Fold.of_fn fst @> Location.iter_temp) srcs ~f:(fun r ->
        on_use (Reg r))
    | Unreachable -> ()
  ;;

  let iter_operands t ~f = iter_operand_use_defs t ~on_def:(fun (x, _) -> f x) ~on_use:f

  let map_operand_use_defs (instr : t) ~on_def ~on_use =
    let map_temp temp ~f =
      f (Operand.Reg temp)
      |> Operand.reg_val
      |> Option.value_exn ~message:"Mapper should not change register operand kind"
    in
    match instr with
    | Nop -> instr
    | Undefined { dst; size } -> Undefined { dst = on_def dst; size }
    | Push { src; size } ->
      let src = map_temp src ~f:on_use in
      Push { src; size }
    | Pop { dst; size } ->
      let dst = map_temp dst ~f:on_def in
      Pop { dst; size }
    | Call p ->
      let dsts =
        (List.map & Tuple2.map_fst & Location.map_temp) ~f:(map_temp ~f:on_def) p.dsts
      in
      let args =
        (List.map & Tuple2.map_fst & Location.map_temp) p.args ~f:(map_temp ~f:on_use)
      in
      Call { p with dsts; args }
    | Block_params params ->
      let mapper =
        List.map & Traverse.of_field Block_param.Fields.param & Location.map_temp
      in
      let params = mapper params ~f:(fun x -> map_temp ~f:on_def x) in
      Block_params params
    | Jump bc ->
      let bc =
        (Traverse.of_field Block_call.Fields.args & List.map & Location.map_temp)
          bc
          ~f:(map_temp ~f:on_use)
      in
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
      let b1 =
        (Traverse.of_field Block_call.Fields.args & List.map & Location.map_temp)
          b1
          ~f:(map_temp ~f:on_use)
      in
      let b2 =
        (Traverse.of_field Block_call.Fields.args & List.map & Location.map_temp)
          b2
          ~f:(map_temp ~f:on_use)
      in
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
    | Ret p ->
      let srcs =
        (List.map & Tuple2.map_fst & Location.map_temp) p.srcs ~f:(map_temp ~f:on_use)
      in
      Ret { p with srcs }
    | Unreachable -> Unreachable
  ;;

  let iter_uses instr ~f =
    iter_operand_use_defs
      instr
      ~on_def:(fun (o, _ty) -> Operand.iter_mem_uses o ~f)
      ~on_use:(fun o -> Operand.iter_any_regs o ~f)
  ;;

  let iter_constrained_uses_exn t ~f =
    match t with
    | Ret { srcs; call_conv } -> begin
      let zipped, _rem =
        List.zip_with_remainder (List.map srcs ~f:fst) call_conv.return_regs
      in
      List.iter zipped ~f:(fun (loc, ty) ->
        Location.iter_temp loc ~f:(fun temp -> f (temp, ty)))
    end
    | Call { dsts = _; args; func = _; call_conv } ->
      let zipped, _rem =
        List.zip_with_remainder (List.map args ~f:fst) call_conv.call_args
      in
      List.iter zipped ~f:(fun (loc, ty) ->
        Location.iter_temp loc ~f:(fun temp -> f (temp, ty)))
    | Bin { dst = _; op; src1; src2 } -> begin
      match op with
      | Lshift | Rshift -> begin
        match src2 with
        | Reg src2 -> f (src2, Mach_reg.RCX)
        | _ -> ()
      end
      | Idiv | Imod -> begin
        match src1 with
        | Reg src1 -> f (src1, Mach_reg.RAX)
        | _ ->
          raise_s
            [%message "Idiv should be legalized so that src1 is reg" (src1 : Operand.t)]
      end
      | _ -> ()
    end
    | _ -> ()
  ;;

  let iter_constrained_defs_exn t ~f =
    match t with
    | Call { dsts; call_conv; _ } ->
      let t, rem = List.zip_with_remainder (List.map ~f:fst dsts) call_conv.return_regs in
      begin
        match rem with
        | Some (First _) -> todol [%here]
        | _ -> ()
      end;
      List.iter t ~f:(fun (loc, mach_reg) ->
        Location.iter_temp loc ~f:(fun temp -> f (temp, mach_reg)));
      ()
    | Bin { dst; op; src1 = _; src2 = _ } ->
      (match op with
       | Idiv | Imod ->
         (match dst with
          | Reg dst -> f (dst, Mach_reg.RAX)
          | _ ->
            raise_s
              [%message "Idiv should be legalized so that dst is reg" (dst : Operand.t)])
       (* for Imul we can use the two operand version *)
       | _ -> ())
    | _ -> ()
  ;;

  let iter_clobbers t ~f =
    match t with
    | Call { dsts; call_conv; _ } ->
      let num_return = List.length dsts in
      assert (num_return <= List.length call_conv.return_regs);
      let return_regs = List.take call_conv.return_regs num_return in
      List.iter call_conv.call_clobbers
      |> Iter.filter ~f:(fun reg -> not (List.mem ~equal:Mach_reg.equal return_regs reg))
      |> Iter.iter ~f
    | Bin { op = Idiv | Imod; _ } ->
      f Mach_reg.RDX;
      ()
    | _ -> ()
  ;;

  let iter_uses_with_known_ty t ~f =
    match t with
    | Undefined _ | Block_params _ | Nop | Unreachable | Jump _ | Cond_jump _ | Pop _ ->
      ()
    | Push { src; size } -> f (src, size)
    | Call { args; _ } ->
      List.iter args ~f:(fun (loc, ty) ->
        Location.iter_temp loc ~f:(fun temp -> f (temp, ty)))
    | Mov { dst = _; src; size } ->
      Operand.iter_reg_val src ~f:(fun temp -> f (temp, size))
    | Mov_abs { dst = _; src = _ } -> ()
    | Bin { dst = _; src1; op; src2 } ->
      let ty =
        match op with
        | And ty | Or ty | Xor ty | Eq ty -> ty
        | Add | Sub | Imul | Idiv | Imod | Lshift | Rshift | Lt | Gt | Le | Ge -> Qword
      in
      Operand.iter_reg_val src1 ~f:(fun temp -> f (temp, ty));
      Operand.iter_reg_val src2 ~f:(fun temp -> f (temp, ty))
    | Ret { srcs; call_conv = _ } ->
      List.iter srcs ~f:(fun (loc, ty) ->
        Location.iter_temp loc ~f:(fun temp -> f (temp, ty)))
  ;;

  let iter_defs instr ~f =
    iter_operand_use_defs
      instr
      ~on_def:(fun (o, _ty) -> Operand.iter_reg_val o ~f)
      ~on_use:(Fn.const ())
  ;;

  let iter_defs_with_ty t ~f =
    iter_operand_use_defs
      t
      ~on_def:(fun (o, ty) -> Operand.iter_reg_val o ~f:(fun temp -> f (temp, ty)))
      ~on_use:(Fn.const ())
  ;;

  let iter_temps t ~f =
    iter_uses t ~f;
    iter_defs t ~f;
    ()
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
    | Call { call_conv; _ } ->
      List.iter call_conv.call_clobbers ~f;
      List.iter call_conv.return_regs ~f;
      List.iter call_conv.call_args ~f;
      ()
    | Undefined _
    | Push _
    | Pop _
    | Nop
    | Block_params _
    | Jump _
    | Cond_jump _
    | Mov _
    | Mov_abs _ -> ()
    | Ret { srcs = _; call_conv } ->
      List.iter call_conv.return_regs ~f;
      ()
    | Unreachable -> ()
  ;;

  let map_uses instr ~f =
    map_operand_use_defs
      instr
      ~on_def:(function
        | Mem address -> Mem (Address.map_uses address ~f)
        | o -> o)
      ~on_use:(function
        | Mem address -> Mem (Address.map_uses address ~f)
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

  let is_jump = function
    | Unreachable | Jump _ | Cond_jump _ -> true
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

  let map_location_use_defs (t : t) ~on_use ~on_def =
    match t with
    | Unreachable
    | Block_params _
    | Nop
    | Mov _
    | Undefined _
    | Push _
    | Pop _
    | Mov_abs _
    | Bin _ -> t
    | Jump _ | Cond_jump _ ->
      (map_block_calls & Block_call.map_args & List.map) t ~f:on_use
    | Ret ({ srcs; call_conv = _ } as p) ->
      let srcs = (List.map & Tuple2.map_fst) srcs ~f:on_use in
      Ret { p with srcs }
    | Call ({ args; func = _; dsts; call_conv = _ } as p) ->
      let args = (List.map & Tuple2.map_fst) args ~f:on_use in
      let dsts = (List.map & Tuple2.map_fst) dsts ~f:on_def in
      Call { p with args; dsts }
  ;;

  let map_location_uses t ~f = map_location_use_defs t ~on_use:f ~on_def:Fn.id
  let map_location_defs t ~f = map_location_use_defs t ~on_use:Fn.id ~on_def:f
  let map_locations t ~f = map_location_use_defs t ~on_use:f ~on_def:f

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

module Stack_builder = struct
  type t =
    { mutable stack_slot_gen : int
    ; mutable stack_slots : (Stack_slot.t * Ty.t) list
    ; first_id : int
    }
  [@@deriving sexp_of]

  let alloc ?(name = "stack_slot") ?info t ty =
    let ident = Stack_slot.create ?info name t.stack_slot_gen in
    t.stack_slot_gen <- t.stack_slot_gen + 1;
    t.stack_slots <- (ident, ty) :: t.stack_slots;
    ident
  ;;
end

module Mach_reg_gen = struct
  type t =
    { table : (Mach_reg.t, Temp.t) Hashtbl.t
    ; mutable id : int
    ; allocation : int Temp.Table.t option
    }

  let get t mach_reg =
    Hashtbl.find_or_add t.table mach_reg ~default:(fun () ->
      let ident = Temp.create (Mach_reg.to_string mach_reg) t.id in
      Option.iter t.allocation ~f:(fun allocation ->
        allocation.Temp.!(ident) <- Mach_reg.to_enum mach_reg);
      t.id <- t.id + 1;
      ident)
  ;;
end

module Ann = struct
  type t = None [@@deriving sexp_of]

  let default = None
end
