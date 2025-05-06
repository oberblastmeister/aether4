open Std
module Token = Ae_c0_token
module Cst = Ae_c0_cst
module Span = Ae_span
module Spanned = Ae_spanned
module Context = Ae_c0_parser_context
open Ae_trace

module Stream_token = struct
  type t = Token.t Spanned.t [@@deriving sexp_of, compare, equal]

  let compare t1 t2 = Token.compare t1.Spanned.t t2.Spanned.t
  let equal t1 t2 = Token.equal t1.Spanned.t t2.Spanned.t
end

module Stream = struct
  open struct
    module Token' = Token
  end

  module Token = Stream_token

  module Chunk = struct
    type t = Token.t array [@@deriving sexp_of, compare, equal]
  end

  module Snapshot = struct
    type t =
      { pos : int
      ; context : Context.t
      }
    [@@deriving sexp_of]
  end

  type t =
    { tokens : Token.t array
    ; mutable pos : int
    ; mutable context : Context.t
    }
  [@@deriving sexp_of]

  let sexp t =
    let tokens =
      Array.sub t.tokens ~pos:t.pos ~len:(min 5 (Array.length t.tokens - t.pos - 1))
    in
    [%message (tokens : Token.t array)]
  ;;

  let next ({ tokens; pos; context } as stream) =
    if pos < Array.length tokens
    then begin
      let t = tokens.(pos) in
      stream.pos <- succ pos;
      Some
        (match t.t with
         | Ident s when Context.is_ty_ident context s -> { t with t = Token'.TyIdent s }
         | _ -> t)
    end
    else None
  ;;

  let peek { tokens; pos; context } =
    if pos < Array.length tokens
    then begin
      let t = tokens.(pos) in
      Some
        (match t.t with
         | Ident s when Context.is_ty_ident context s -> { t with t = Token'.TyIdent s }
         | _ -> t)
    end
    else None
  ;;

  let snapshot { pos; context; _ } = { Snapshot.pos; context }

  let restore t { Snapshot.pos; context } =
    t.pos <- pos;
    t.context <- context
  ;;

  let create tokens = { tokens; pos = 0; context = Context.empty }
end

module Error = struct
  type t = Sexp of Sexp.t [@@deriving sexp_of]
end

module Parser = Parsec.Make (struct
    module Data = Unit
    module Stream = Stream
    module Error = Error
  end)

open Parser.Syntax

let expect_eq token env =
  Parser.expect (fun t -> if Token.equal t.t token then Some t else None) env
;;

let expect_eq_ token env =
  Parser.expect (fun t -> if Token.equal t.t token then Some t else None) env |> ignore
;;

let parse_ident env = Parser.expect (Spanned.map_option ~f:Token.ident_val) env
let parse_var env = (Cst.var <$> parse_ident) env
let parse_ty_ident env = Parser.expect (Spanned.map_option ~f:Token.tyident_val) env
let parse_general_ident env = (parse_ident <|> parse_ty_ident) env

let spanned_delim ~left ~right p env =
  let delim = expect_eq left env in
  let res = p env in
  let delim =
    expect_eq right
    |> Parser.cut (Sexp [%message "expected closing " ~delimiter:(delim.t : Token.t)])
    |> Fn.( |> ) env
  in
  { Spanned.t = res; span = Span.Syntax.(delim.span ++ delim.span) }
;;

let spanned_parens p env = spanned_delim ~left:LParen ~right:RParen p env
let spanned_braces p env = spanned_delim ~left:LBrace ~right:RBrace p env
let parens p env = (Spanned.value <$> spanned_parens p) env
let braces p env = (Spanned.value <$> spanned_braces p) env

let chainl ~expr:parse_expr ~op:parse_op ~f env =
  let rec loop lhs env =
    ((fun env ->
       let op = parse_op env in
       let rhs = parse_expr env in
       loop (f ~lhs ~op ~rhs) env)
     <|> Parser.pure lhs)
      env
  in
  let lhs = parse_expr env in
  loop lhs env
;;

let chain_post ~post x env =
  let rec loop t env = ((fun env -> loop (post t env) env) <|> Parser.pure t) env in
  loop x env
;;

let scoped p env =
  let stream = Parser.stream env in
  let context = stream.context in
  let res = p env in
  stream.context <- context;
  res
;;

let declare_ident env (ident : Cst.var) =
  let stream = Parser.stream env in
  stream.context <- Context.declare_ident ident.t stream.context
;;

let declare_ty_ident env (ident : Cst.var) =
  let stream = Parser.stream env in
  stream.context <- Context.declare_ty_ident ident.t stream.context
;;

let parse_int env =
  let int = expect_eq Int env in
  Cst.Int int.span
;;

let parse_bool env =
  let bool = expect_eq Bool env in
  Cst.Bool bool.span
;;

let parse_void env =
  let void = expect_eq Void env in
  Cst.Void void.span
;;

let parse_ty_var env =
  let var = parse_ty_ident env in
  Cst.Ty_var var
;;

let parse_struct_ty env =
  let struct_tok = expect_eq Struct env in
  let name = parse_general_ident env in
  Cst.Ty_struct { name; span = Span.Syntax.(struct_tok.span ++ name.span) }
;;

let parse_field_suffix env =
  let deref = (false <$ expect_eq_ Dot <|> (true <$ expect_eq_ DashLangle)) env in
  deref, parse_general_ident env
;;

let rec parse_atom_ty env =
  (parse_struct_ty <|> parse_int <|> parse_bool <|> parse_void <|> parse_ty_var) env

and parse_ty env =
  let ty = parse_atom_ty env in
  let post ty env =
    let pointer env =
      let tok = expect_eq Star env in
      Cst.Pointer { ty; span = Span.Syntax.(Cst.ty_span ty ++ tok.span) }
    in
    let array env =
      let tok = expect_eq LBrackRBrack env in
      Cst.Array { ty; span = Span.Syntax.(Cst.ty_span ty ++ tok.span) }
    in
    (pointer <|> array) env
  in
  chain_post ~post ty env
;;

let rec parse_block env : Cst.block =
  let open Span.Syntax in
  let lbrace = expect_eq LBrace env in
  let block = Parser.many try_parse_stmt |> scoped |> Fn.( |> ) env in
  let rbrace =
    expect_eq RBrace
    |> Parser.cut (Sexp [%message "expected closing brace for block"])
    |> Fn.( |> ) env
  in
  { block; span = lbrace.span ++ rbrace.span }

and try_parse_stmt env : Cst.stmt =
  let stmt =
    (parse_if
     <|> parse_for
     <|> parse_while
     <|> ((fun b -> Cst.Block b) <$> parse_block)
     <|> parse_semi_stmt)
      env
  in
  stmt

and parse_stmt env =
  try_parse_stmt |> Parser.cut (Sexp [%message "expected stmt"]) |> Fn.( |> ) env

and parse_semi_stmt env : Cst.stmt =
  let res =
    (parse_assign
     <* expect_eq_ Semi
     <|> (parse_return <* expect_eq_ Semi)
     <|> (parse_assert <* expect_eq_ Semi)
     <|> (parse_post <* expect_eq_ Semi)
     <|> (parse_effect <* expect_eq_ Semi)
     <|> (parse_decl <* expect_eq_ Semi)
     <|> parse_single_semi_stmt)
      env
  in
  res

and parse_single_semi_stmt env =
  expect_eq_ Semi env;
  Cst.Assert { expr = Bool_const { t = true; span = Span.none }; span = Span.none }

and parse_effect env =
  let e = try_parse_expr env in
  Cst.Effect e

and parse_simp env : Cst.stmt =
  (parse_assign <|> parse_post <|> parse_effect <|> parse_decl) env

and parse_if env : Cst.stmt =
  let open Span.Syntax in
  let if_tok = expect_eq If env in
  let cond =
    parens parse_expr
    |> Parser.cut (Sexp [%message "expected condition expression for if"])
    |> Fn.( |> ) env
  in
  let body1 = scoped parse_stmt env in
  let body2 =
    Parser.optional
      (fun env ->
         expect_eq_ Else env;
         scoped parse_stmt env)
      env
  in
  Cst.If
    { cond
    ; body1
    ; body2
    ; span =
        if_tok.span
        ++ Option.value_map
             body2
             ~f:(fun body2 -> Cst.stmt_span body1 ++ Cst.stmt_span body2)
             ~default:(Cst.stmt_span body1)
    }

and parse_while env : Cst.stmt =
  let open Span.Syntax in
  let while_tok = expect_eq While env in
  let cond =
    parens parse_expr
    |> Parser.cut (Sexp [%message "expected while condition expression"])
    |> Fn.( |> ) env
  in
  let body = scoped parse_stmt env in
  Cst.While { cond; body; span = while_tok.span ++ Cst.stmt_span body }

and parse_for env : Cst.stmt =
  let open Span.Syntax in
  scoped
    (fun env ->
       let for_tok = expect_eq For env in
       let paren =
         parens parse_for_paren
         |> Parser.cut (Sexp [%message "expected for parens"])
         |> Fn.( |> ) env
       in
       let body = scoped parse_stmt env in
       Cst.For { paren; body; span = for_tok.span ++ Cst.stmt_span body })
    env

and parse_for_paren env : Cst.for_paren =
  let init = (Parser.optional parse_simp) env in
  expect_eq_ Semi env;
  let cond = (Parser.optional try_parse_expr) env in
  expect_eq_ Semi env;
  let incr = (Parser.optional parse_simp) env in
  { init; cond; incr }

and parse_assert env : Cst.stmt =
  let assert_tok = expect_eq Assert env in
  let expr = parse_expr env in
  Cst.Assert { expr; span = Span.Syntax.(assert_tok.span ++ Cst.expr_span expr) }

and parse_return env : Cst.stmt =
  let open Span.Syntax in
  let ret = expect_eq Return env in
  let expr = Parser.optional try_parse_expr env in
  Cst.Return
    { expr
    ; span =
        Option.value_map
          expr
          ~f:(fun expr -> ret.span ++ Cst.expr_span expr)
          ~default:ret.span
    }

and parse_decl env : Cst.stmt =
  let open Span.Syntax in
  let ty = parse_ty env in
  let names = Parser.sep parse_general_ident ~by:(expect_eq_ Comma) env in
  let expr =
    Parser.optional
      (fun env ->
         expect_eq_ Eq env;
         let expr = parse_expr env in
         expr)
      env
  in
  List.iter names ~f:(declare_ident env);
  Cst.Decl
    { ty
    ; names
    ; expr
    ; span =
        Cst.ty_span ty ++ Option.value_map expr ~f:Cst.expr_span ~default:(Cst.ty_span ty)
    }

and parse_post env : Cst.stmt =
  let open Span.Syntax in
  let lvalue = parse_lvalue env in
  let op = parse_post_op env in
  Cst.Post { lvalue; op = op.t; span = Cst.expr_span lvalue ++ op.span }

and parse_post_op env : Cst.post_op Spanned.t =
  ((Parser.map & Spanned.map) (expect_eq PlusPlus) ~f:(Fn.const Cst.Incr)
   <|> (Parser.map & Spanned.map) (expect_eq DashDash) ~f:(Fn.const Cst.Decr))
    env

and parse_assign env : Cst.stmt =
  let open Span.Syntax in
  let lvalue = parse_lvalue env in
  let op = parse_assign_op env in
  let expr = parse_expr env in
  Cst.Assign { lvalue; op; expr; span = Cst.expr_span lvalue ++ Cst.expr_span expr }

and parse_assign_op env : Cst.assign_op =
  (Cst.Mul_assign
   <$ expect_eq_ StarEq
   <|> (Cst.Add_assign <$ expect_eq_ PlusEq)
   <|> (Cst.Sub_assign <$ expect_eq_ DashEq)
   <|> (Cst.Mod_assign <$ expect_eq_ PercentEq)
   <|> (Cst.Div_assign <$ expect_eq_ SlashEq)
   <|> (Cst.Bit_and_assign <$ expect_eq_ AmpersandEq)
   <|> (Cst.Bit_or_assign <$ expect_eq_ PipeEq)
   <|> (Cst.Bit_xor_assign <$ expect_eq_ CaretEq)
   <|> (Cst.Lshift_assign <$ expect_eq_ LangleLangleEq)
   <|> (Cst.Rshift_assign <$ expect_eq_ RangleRangleEq)
   <|> (Cst.Id_assign <$ expect_eq_ Eq))
    env

and parse_lvalue env : Cst.expr =
  let lvalue = (parens parse_lvalue <|> parse_lvalue_deref <|> parse_lvalue_post) env in
  chain_post ~post:parse_post_chain lvalue env

and parse_lvalue_post env : Cst.expr =
  let lvalue = parse_var env in
  chain_post ~post:parse_post_chain lvalue env

and parse_post_chain expr env = (parse_post_field expr <|> parse_post_index expr) env

and parse_post_field expr env =
  let deref, field = parse_field_suffix env in
  Cst.Field_access
    { expr; field; span = Span.Syntax.(Cst.expr_span expr ++ field.span); deref }

and parse_post_index expr env =
  let _lbrack = expect_eq LBrack env in
  let index = parse_expr env in
  let rbrack = expect_eq RBrack env in
  Cst.Index { expr; index; span = Span.Syntax.(Cst.expr_span expr ++ rbrack.span) }

and parse_lvalue_deref env : Cst.expr =
  let star = expect_eq Star env in
  let expr = parse_lvalue env in
  Cst.Deref { expr; span = Span.Syntax.(star.span ++ Cst.expr_span expr) }

and parse_expr env : Cst.expr =
  let expr =
    try_parse_expr |> Parser.cut (Sexp [%message "expected expression"]) |> Fn.( |> ) env
  in
  expr

and try_parse_expr env : Cst.expr =
  let expr = parse_ternary env in
  expr

and parse_ternary env : Cst.expr =
  let open Span.Syntax in
  let cond = parse_log_or env in
  ((fun env ->
     expect_eq_ Question env;
     let then_expr = parse_expr env in
     expect_eq_ Colon
     |> Parser.cut (Sexp [%message "expected colon in ternary"])
     |> Fn.( |> ) env;
     let else_expr = parse_expr env in
     Cst.Ternary
       { cond
       ; then_expr
       ; else_expr
       ; span = Cst.expr_span cond ++ Cst.expr_span then_expr ++ Cst.expr_span else_expr
       })
   <|> Parser.pure cond)
    env

and parse_log_or env : Cst.expr =
  chainl ~expr:parse_log_and ~op:(Cst.Log_or <$ expect_eq_ PipePipe) ~f:Cst.bin env

and parse_log_and env : Cst.expr =
  chainl
    ~expr:parse_bit_or
    ~op:(Cst.Log_and <$ expect_eq_ AmpersandAmpersand)
    ~f:Cst.bin
    env

and parse_bit_or env : Cst.expr =
  chainl ~expr:parse_bit_xor ~op:(Cst.Bit_or <$ expect_eq_ Pipe) ~f:Cst.bin env

and parse_bit_xor env : Cst.expr =
  chainl ~expr:parse_bit_and ~op:(Cst.Bit_xor <$ expect_eq_ Caret) ~f:Cst.bin env

and parse_bit_and env : Cst.expr =
  chainl ~expr:parse_equality ~op:(Cst.Bit_and <$ expect_eq_ Ampersand) ~f:Cst.bin env

and parse_equality env : Cst.expr =
  chainl
    ~expr:parse_comparison
    ~op:(Cst.Eq <$ expect_eq_ EqEq <|> (Cst.Neq <$ expect_eq_ BangEq))
    ~f:Cst.bin
    env

and parse_comparison env : Cst.expr =
  chainl
    ~expr:parse_shift
    ~op:
      (Cst.Lt
       <$ expect_eq_ Langle
       <|> (Cst.Gt <$ expect_eq_ Rangle)
       <|> (Cst.Le <$ expect_eq_ LangleEq)
       <|> (Cst.Ge <$ expect_eq_ RangleEq))
    ~f:Cst.bin
    env

and parse_shift env : Cst.expr =
  chainl
    ~expr:parse_add
    ~op:(Cst.Lshift <$ expect_eq_ LangleLangle <|> (Cst.Rshift <$ expect_eq_ RangleRangle))
    ~f:Cst.bin
    env

and parse_add env : Cst.expr =
  chainl
    ~expr:parse_mul
    ~op:(Cst.Add <$ expect_eq_ Plus <|> (Cst.Sub <$ expect_eq_ Dash))
    ~f:Cst.bin
    env

and parse_mul env : Cst.expr =
  chainl
    ~expr:parse_unary_expr
    ~op:
      (Cst.Mul
       <$ expect_eq_ Star
       <|> (Cst.Div <$ expect_eq_ Slash)
       <|> (Cst.Mod <$ expect_eq_ Percent))
    ~f:Cst.bin
    env

and parse_unary_expr env : Cst.expr =
  let open Span.Syntax in
  ((fun env ->
     let dash = expect_eq Dash env in
     let expr = parse_unary_expr env in
     Cst.Unary { op = Neg; expr; span = dash.span ++ Cst.expr_span expr })
   <|> (fun env ->
   let tilde = expect_eq Tilde env in
   let expr = parse_unary_expr env in
   Cst.Unary { op = Bit_not; expr; span = tilde.span ++ Cst.expr_span expr })
   <|> (fun env ->
   let star = expect_eq Star env in
   let expr = parse_unary_expr env in
   Deref { expr; span = star.span ++ Cst.expr_span expr })
   <|> (fun env ->
   let bang = expect_eq Bang env in
   let expr = parse_unary_expr env in
   Cst.Unary { op = Log_not; expr; span = bang.span ++ Cst.expr_span expr })
   <|> parse_expr_post)
    env

and parse_expr_post env : Cst.expr =
  let expr = parse_atom env in
  chain_post ~post:parse_post_chain expr env

and parse_atom env : Cst.expr =
  ((fun d -> Cst.Int_const d)
   <$> parse_num
   <|> parse_null
   <|> parens parse_expr
   <|> (Cst.bool_const <$> parse_true)
   <|> (Cst.bool_const <$> parse_false)
   <|> parse_alloc
   <|> parse_alloc_array
   <|> parse_call
   <|> parse_var)
    env

and parse_alloc env : Cst.expr =
  let alloc = expect_eq Alloc env in
  let ty = spanned_parens parse_ty env in
  Cst.Alloc { ty = ty.t; span = Span.Syntax.(alloc.span ++ ty.span) }

and parse_alloc_array env : Cst.expr =
  let alloc_array = expect_eq AllocArray env in
  let { Spanned.t = ty, expr; span = parens_span } =
    spanned_parens
      (fun env ->
         let ty = parse_ty env in
         expect_eq_ Comma env;
         let expr = parse_expr env in
         ty, expr)
      env
  in
  Cst.Alloc_array
    { ty
    ; expr
    ; span =
        Span.Syntax.(
          alloc_array.span ++ Cst.ty_span ty ++ Cst.expr_span expr ++ parens_span)
    }

and parse_null env : Cst.expr =
  let null = expect_eq Null env in
  Null null.span

and parse_args env : Cst.expr list = Parser.sep try_parse_expr ~by:(expect_eq_ Comma) env

and parse_call env : Cst.expr =
  let func = parse_ident env in
  let args = spanned_parens parse_args env in
  Call { func; args = args.t; span = Span.Syntax.(func.span ++ args.span) }

and parse_true = (Parser.map & Spanned.map) (expect_eq True) ~f:(Fn.const true)
and parse_false = (Parser.map & Spanned.map) (expect_eq False) ~f:(Fn.const false)

and parse_num env : Z.t Spanned.t =
  (Parser.map & Spanned.map)
    ~f:(fun s ->
      Z.of_string s
      |> Option.value_or_thunk ~default:(fun () ->
        Parser.error env (Sexp [%message "invalid number"])))
    (Parser.expect (Spanned.map_option ~f:Token.hexnum_val)
     <|> Parser.expect (Spanned.map_option ~f:Token.decnum_val))
    env
;;

let parse_param env : Cst.param =
  let open Span.Syntax in
  let ty = parse_ty env in
  let var = parse_general_ident env in
  declare_ident env var;
  { ty; var; span = Cst.ty_span ty ++ var.span }
;;

let parse_params env : Cst.param list Spanned.t =
  spanned_parens (Parser.sep parse_param ~by:(expect_eq_ Comma)) env
;;

let parse_func env : Cst.global_decl =
  let is_extern = Parser.optional (expect_eq_ Extern) env |> Option.is_some in
  let ty = parse_ty env in
  let name = parse_general_ident env in
  let params, body =
    scoped
      (fun env ->
         let params = parse_params env in
         let body = (Option.some <$> parse_block <|> (None <$ expect_eq_ Semi)) env in
         params, body)
      env
  in
  let open Span.Syntax in
  Cst.Func
    { is_extern
    ; ty
    ; name
    ; params = params.t
    ; body
    ; span =
        Cst.ty_span ty
        ++ Option.value_map body ~f:(fun body -> body.span) ~default:params.span
    }
;;

let parse_field env : Cst.field =
  let ty = parse_ty env in
  let name = parse_general_ident env in
  let semi = expect_eq Semi env in
  { name; ty; span = Span.Syntax.(Cst.ty_span ty ++ name.span ++ semi.span) }
;;

let parse_struct_part env : Cst.strukt =
  let fields = spanned_braces (Parser.many parse_field) env in
  { fields = fields.t; span = fields.span }
;;

let parse_struct env : Cst.global_decl =
  let strukt_tok = expect_eq Struct env in
  Parser.cut
    (Sexp [%message "invalid struct"])
    (fun env ->
       let name = parse_general_ident env in
       let strukt = Parser.optional parse_struct_part env in
       expect_eq_ Semi env;
       Cst.Struct
         { name
         ; strukt
         ; span =
             Span.Syntax.(
               let span = strukt_tok.span ++ name.span in
               span
               ++ Option.value_map
                    strukt
                    ~f:(fun strukt -> span ++ strukt.span)
                    ~default:span)
         })
    env
;;

let parse_typedef env : Cst.global_decl =
  let typedef = expect_eq Typedef env in
  Parser.cut
    (Sexp [%message "invalid typedef"])
    (fun env ->
       let name = parse_general_ident env in
       let ty = parse_ty env in
       expect_eq_ Semi env;
       declare_ty_ident env name;
       Cst.Typedef { ty; name; span = Span.Syntax.(typedef.span ++ name.span) })
    env
;;

let parse_global_decl env : Cst.global_decl =
  (* the func has to go first because it conflicts with struct but takes precedence *)
  (parse_func <|> parse_struct <|> parse_typedef) env
;;

let rec parse_program env : Cst.program =
  ((fun env ->
     let res = Parser.many parse_global_decl env in
     expect_eq_ Eof env;
     res)
   |> Parser.cut (Sexp [%message "invalid program"]))
    env
;;

let parse tokens =
  let stream = Stream.create tokens in
  Parser.with_env () stream (fun env -> parse_program env)
  |> Parsec.Parse_result.to_result_exn
  |> Result.map_error ~f:(fun (Sexp s, t) ->
    Core.Error.create_s s |> Core.Error.tag_s ~tag:[%sexp (t : Token.t Spanned.t option)])
;;
