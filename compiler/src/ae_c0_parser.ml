open Std
module Token = Ae_c0_token
module Cst = Ae_c0_cst
module Stream = Parsec.Make_stream (Token)

module Error = struct
  type t = Sexp of Sexp.t [@@deriving sexp_of]
end

module Parser = Parsec.Make (struct
    module Stream = Stream
    module Error = Error
  end)

open Parser.Syntax

let parse_ty env : Cst.ty =
  (Parser.expect_eq Int $> Cst.Int <|> (Parser.expect_eq Bool $> Cst.Bool)) env
;;

let parse_ident env = Parser.expect Token.ident_val env

let parens p env =
  Parser.expect_eq LParen env;
  let res = p env in
  Parser.expect_eq RParen
  |> Parser.cut (Sexp [%message "expected closing RParen"])
  |> Fn.( |> ) env;
  res
;;

let rec parse_program env : Cst.program =
  ((fun env ->
     let ty =
       (Parser.cut (Sexp [%message "expected return type for function"]) parse_ty) env
     in
     let name = parse_ident env in
     Parser.expect_eq LParen env;
     Parser.expect_eq RParen env;
     let block = parse_block env in
     ({ ty; name; block } : Cst.program))
   |> Parser.cut (Sexp [%message "invalid program"]))
    env

and parse_block env : Cst.block =
  Parser.expect_eq LBrace env;
  let stmts = Parser.many parse_stmt env in
  Parser.expect_eq RBrace
  |> Parser.cut
       (Sexp
          [%message
            "expected closing brace for block"
              (Stream.peek (Parser.stream env) : Token.t option)])
  |> Fn.( |> ) env;
  { stmts }

and parse_stmt env : Cst.stmt =
  let stmt =
    (parse_if
     <|> parse_for
     <|> parse_while
     <|> ((fun b -> Cst.Block b) <$> parse_block)
     <|> parse_semi_stmt)
      env
  in
  stmt

and parse_semi_stmt env : Cst.stmt =
  let res =
    ((fun d -> Cst.Decl d)
     <$> parse_decl
     <|> ((fun d -> Cst.Assign d) <$> parse_assign)
     <|> ((fun e -> Cst.Return e) <$> parse_return))
      env
  in
  Parser.expect_eq Semi
  |> Parser.cut (Sexp [%message "expected semicolon after statement"])
  |> Fn.( |> ) env;
  res

and parse_if env : Cst.stmt =
  Parser.expect_eq If env;
  let cond =
    parens parse_expr
    |> Parser.cut (Sexp [%message "expected condition expression for if"])
    |> Fn.( |> ) env
  in
  let body1 =
    parse_stmt |> Parser.cut (Sexp [%message "expected if stmt"]) |> Fn.( |> ) env
  in
  let body2 =
    Parser.optional
      (fun env ->
         Parser.expect_eq Else env;
         parse_stmt |> Parser.cut (Sexp [%message "expected if stmt"]) |> Fn.( |> ) env)
      env
  in
  Cst.If { cond; body1; body2 }

and parse_while env : Cst.stmt =
  Parser.expect_eq While env;
  let cond =
    parens parse_expr
    |> Parser.cut (Sexp [%message "expected while condition expression"])
    |> Fn.( |> ) env
  in
  let body =
    parse_stmt |> Parser.cut (Sexp [%message "expected while stmt"]) |> Fn.( |> ) env
  in
  Cst.While { cond; body }

and parse_for env : Cst.stmt =
  Parser.expect_eq For env;
  let paren =
    parens parse_for_paren
    |> Parser.cut (Sexp [%message "expected for parens"])
    |> Fn.( |> ) env
  in
  let body =
    parse_stmt |> Parser.cut (Sexp [%message "expected for stmt"]) |> Fn.( |> ) env
  in
  Cst.For { paren; body }

and parse_for_paren env : Cst.for_paren =
  let init = parse_stmt env in
  let cond = parse_expr env in
  let incr = parse_stmt env in
  { init; cond; incr }

and parse_return env : Cst.expr =
  Parser.expect_eq Return env;
  parse_expr env

and parse_decl env : Cst.decl =
  let ty = parse_ty env in
  let name = parse_ident env in
  let expr =
    Parser.optional
      (fun env ->
         Parser.expect_eq Eq env;
         let expr =
           parse_expr
           |> Parser.cut (Sexp [%message "expected expression after equal sign"])
           |> Fn.( |> ) env
         in
         expr)
      env
  in
  ({ ty; name; expr } : Cst.decl)

and parse_assign env : Cst.assign =
  let lvalue = parse_lvalue env in
  let op = parse_assign_op env in
  let expr = parse_expr env in
  { lvalue; op; expr }

and parse_assign_op env : Cst.assign_op =
  (Cst.MulEq
   <$ Parser.expect_eq StarEq
   <|> (Cst.AddEq <$ Parser.expect_eq PlusEq)
   <|> (Cst.SubEq <$ Parser.expect_eq DashEq)
   <|> (Cst.ModEq <$ Parser.expect_eq PercentEq)
   <|> (Cst.DivEq <$ Parser.expect_eq SlashEq)
   <|> (Cst.Eq <$ Parser.expect_eq Eq))
    env

and parse_lvalue env : Cst.lvalue =
  ((fun env ->
     Parser.expect_eq LParen env;
     let lvalue = parse_lvalue env in
     Parser.expect_eq RParen env;
     lvalue)
   <|> parse_ident)
    env

and parse_expr env : Cst.expr =
  let expr = parse_add env in
  expr

and parse_add env : Cst.expr =
  let lhs = parse_mul env in
  let rec loop lhs env =
    ((fun env ->
       let op =
         (Cst.Add <$ Parser.expect_eq Plus <|> (Cst.Sub <$ Parser.expect_eq Dash)) env
       in
       let rhs =
         parse_mul
         |> Parser.cut (Sexp [%message "expected rhs of expression"])
         |> Fn.( |> ) env
       in
       loop (Cst.Bin { lhs; op; rhs }) env)
     <|> Parser.pure lhs)
      env
  in
  loop lhs env

and parse_mul env : Cst.expr =
  let lhs = parse_unary_expr env in
  let rec loop lhs env =
    ((fun env ->
       let op =
         (Cst.Mul
          <$ Parser.expect_eq Star
          <|> (Cst.Div <$ Parser.expect_eq Slash)
          <|> (Cst.Mod <$ Parser.expect_eq Percent))
           env
       in
       let rhs =
         parse_unary_expr
         |> Parser.cut (Sexp [%message "expected rhs of expression"])
         |> Fn.( |> ) env
       in
       loop (Cst.Bin { lhs; op; rhs }) env)
     <|> Parser.pure lhs)
      env
  in
  loop lhs env

and parse_unary_expr env : Cst.expr =
  ((fun env ->
     Parser.expect_eq Dash env;
     let e = parse_unary_expr env in
     Cst.Neg e)
   <|> parse_atom)
    env

and parse_atom env : Cst.expr =
  ((fun d -> Cst.IntConst d)
   <$> parse_num
   <|> (Parser.expect_eq True $> Cst.BoolConst true)
   <|> (Parser.expect_eq False $> Cst.BoolConst false)
   <|> ((fun v -> Cst.Var v) <$> parse_ident))
    env

and parse_num env : Z.t =
  Parser.map
    (fun s ->
       Z.of_string s
       |> Option.value_or_thunk ~default:(fun () ->
         Parser.error env (Sexp [%message "invalid number"])))
    (Parser.expect Token.hexnum_val <|> Parser.expect Token.decnum_val)
    env
;;

let parse tokens =
  let stream = Stream.of_chunk tokens in
  Parser.with_env stream (fun env -> parse_program env)
  |> Parsec.Parse_result.to_result_exn
  |> Result.map_error ~f:(fun (Sexp s) -> Core.Error.create_s s)
;;
