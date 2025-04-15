open Ppxlib
module Ast = Ast_builder.Default

let bind_fail_expander ~ctxt expr =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  match expr with
  | [%expr
      let [%p? pat] = [%e? rhs] in
      [%e? body]] ->
    [%expr
      match%bind [%e rhs] with
      | [%p pat] -> [%e body]
      | _ -> fail ()]
  | _ ->
    Location.raise_errorf
      ~loc:(Expansion_context.Extension.extension_point_loc ctxt)
      "pattern match must be a let binding"
;;

let fail_expander ~ctxt (expr : expression) =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  match expr with
  | [%expr
      let [%p? pat] = [%e? rhs] in
      [%e? body]] ->
    [%expr
      match [%e rhs] with
      | [%p pat] -> [%e body]
      | _ -> fail ()]
  | _ ->
    Location.raise_errorf
      ~loc:(Expansion_context.Extension.extension_point_loc ctxt)
      "pattern match must be a let binding"
;;

(* let like_expander ~ctxt (modul : module_expr) =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  () *)

let fail_extension =
  Extension.V3.declare
    "fail"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    fail_expander
;;

let bind_fail_extension =
  Extension.V3.declare
    "bind_fail"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    bind_fail_expander
;;

let () =
  Driver.register_transformation
    ~rules:
      [ Context_free.Rule.extension fail_extension
      ; Context_free.Rule.extension bind_fail_extension
      ]
    "ppx_pattern"
;;
