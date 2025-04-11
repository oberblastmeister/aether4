open Std
open Ae_tir_types
module Entity = Ae_entity_std

exception Exn of Error.t

let throw_s s = raise (Exn (Error.create_s s))

let check_temp_ty ty_table temp ty =
  let open Entity.Ident.Table.Syntax in
  if not (Ty.equal ty_table.!(temp) ty)
  then throw_s [%message "Type mismatch" (temp : Temp.t) (ty : Ty.t)]
;;

let check_block_call ty_table func (block_call : Block_call.t) =
  let dst_block = Func.find_block_exn func block_call.label in
  let (`temps block_params) =
    Block.find_block_params dst_block
    |> Instr'.instr
    |> Instr.block_params_val
    |> Option.value_exn
  in
  let block_param_tys = List.map block_params ~f:snd in
  let temp_with_ty =
    match List.zip block_call.args block_param_tys with
    | Ok t -> t
    | Unequal_lengths ->
      throw_s
        [%message
          "Didn't provide the right number of block args"
            (dst_block.label : Label.t)
            (block_call.args : Temp.t list)
            (block_param_tys : Ty.t list)]
  in
  begin
    let@: temp, ty = List.iter temp_with_ty in
    check_temp_ty ty_table temp ty
  end;
  ()
;;

let check_instr ty_table func (instr : Instr.t) =
  match instr with
  | Nop | Block_params _ -> ()
  | Call _ -> todol [%here]
  | Bin { dst = _; op; src1; src2 } ->
    (match op with
     | Add | Sub | Mul | Div | Mod | Lt | Gt | Le | Ge | And | Or | Xor | Lshift | Rshift
       ->
       check_temp_ty ty_table src1 Int;
       check_temp_ty ty_table src2 Int;
       ()
     | Eq ty ->
       check_temp_ty ty_table src1 ty;
       check_temp_ty ty_table src2 ty)
  | Unary { dst = _; op; src } ->
    (match op with
     | Copy ty -> check_temp_ty ty_table src ty)
  | Nullary _ -> ()
  | Jump b -> check_block_call ty_table func b
  | Cond_jump { cond; b1; b2 } ->
    check_temp_ty ty_table cond Bool;
    check_block_call ty_table func b1;
    check_block_call ty_table func b2
  | Ret _ ->
    (* TODO: check the function return type here *)
    ()
  | Unreachable -> ()
;;

let check' (func : Func.t) =
  let ty_table = Func.get_ty_table func in
  let@: block = Func.iter_blocks func in
  let@: instr = Block.iter_fwd block in
  check_instr ty_table func instr.i
;;

let check func =
  let open Or_error.Let_syntax in
  match check' func with
  | exception Exn e -> Error e
  | () -> Ok ()
;;
