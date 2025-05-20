open Std
module Ast = Ae_c0_ast

exception Exn of Error.t

let throw_s s = raise (Exn (Error.create_s s))

let rec block_returns (block : Ast.block) = List.exists block.stmts ~f:stmt_returns

and stmt_returns (stmt : Ast.stmt) =
  match stmt with
  | Return _ -> true
  | Par _ | Break _ | Assert _ | Assign _ | Declare _ | While _ | Effect _ -> false
  | Block block -> block_returns block
  | If { cond = _; body1; body2 = Some body2; span = _ } ->
    stmt_returns body1 && stmt_returns body2
  | If { cond = _; body1 = _; body2 = None; span = _ } -> false
;;

let check_global_decl (global_decl : Ast.global_decl) =
  match global_decl with
  | Ast.Extern_func_defn _ | Ast.Func_decl _ | Ast.Typedef _ | Ast.Struct _ -> ()
  | Ast.Func_defn func ->
    let does_return = block_returns func.body in
    if not does_return
    then
      throw_s
        [%message
          "Function does not return on all control flow paths" (func.name : Ast.var)]
;;

let check_program (program : Ast.program) = List.iter program ~f:check_global_decl

let check_program (program : Ast.program) =
  match check_program program with
  | exception Exn e -> Error e
  | () -> Ok ()
;;
