open Std
open Ae_c0_ast
module Free_vars = Ae_c0_ast_free_vars

let rec convert_stmt (stmt : stmt) : stmt =
  match stmt with
  | If p ->
    let body1 = convert_stmt p.body1 in
    let body2 = Option.map p.body2 ~f:convert_stmt in
    If { p with body1; body2 }
  | Block block -> Block (convert_block block)
  | While p ->
    let body = convert_stmt p.body in
    While { p with body }
  | Assign _ | Declare _ | Effect _ | Return _ | Assert _ | Break _ -> stmt
  | Par { block1; block2; block1_free_vars = _; block2_free_vars = _; span } ->
    let block1_free_vars = Free_vars.block_free_vars block1 in
    let block2_free_vars = Free_vars.block_free_vars block2 in
    let block1 = convert_block block1 in
    let block2 = convert_block block2 in
    Par { block1; block2; block1_free_vars; block2_free_vars; span }

and convert_block (block : block) : block =
  let stmts = List.map block.stmts ~f:convert_stmt in
  { block with stmts }
;;

let convert_func_defn (func : func_defn) : func_defn =
  let body = convert_block func.body in
  { func with body }
;;

let convert_program (prog : program) : program =
  List.map prog ~f:(fun decl ->
    match decl with
    | Extern_func_defn _ | Func_decl _ -> decl
    | Func_defn func -> Func_defn (convert_func_defn func)
    | Typedef _ | Struct _ -> decl)
;;
