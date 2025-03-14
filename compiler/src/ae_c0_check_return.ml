open Std
module Ast = Ae_c0_ast

let rec block_returns (block : Ast.block) = List.exists block ~f:stmt_returns

and stmt_returns (stmt : Ast.stmt) =
  match stmt with
  | Return _ -> true
  | Assign _ | Declare _ | While _ | Effect _ -> false
  | Block stmts -> block_returns stmts
  | If { cond = _; body1; body2 = Some body2 } -> stmt_returns body1 && stmt_returns body2
  | If { cond = _; body1 = _; body2 = None } -> false
;;

let check_program (program : Ast.program) =
  let does_return = block_returns program.block in
  if does_return
  then Ok ()
  else error_s [%message "Program does not return on all control flow paths"]
;;
