open Std
module Token = Ae_c0_token
module Cst = Ae_c0_cst
module Stream = Parsec.Make_stream (Token)
open Ae_trace

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
     <|> (parse_semi_stmt <* Parser.expect_eq Semi))
      env
  in
  stmt

and parse_semi_stmt env : Cst.stmt =
  let res =
    ((fun d -> Cst.Decl d)
     <$> parse_decl
     <|> ((fun d -> Cst.Assign d) <$> parse_assign)
     <|> ((fun e -> Cst.Return e) <$> parse_return)
     <|> parse_post
     <|> ((fun e -> Cst.Effect e) <$> parse_expr))
      env
  in
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
  let init = (Parser.optional parse_semi_stmt) env in
  Parser.expect_eq Semi env;
  let cond = parse_expr env in
  Parser.expect_eq Semi env;
  let incr = (Parser.optional parse_semi_stmt) env in
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

and parse_post env : Cst.stmt =
  let lvalue = parse_lvalue env in
  let op = parse_post_op env in
  Cst.Post { lvalue; op }

and parse_post_op env : Cst.post_op =
  (Cst.Incr <$ Parser.expect_eq PlusPlus <|> (Cst.Decr <$ Parser.expect_eq DashDash)) env

and parse_assign env : Cst.assign =
  let lvalue = parse_lvalue env in
  let op = parse_assign_op env in
  let expr = parse_expr env in
  { lvalue; op; expr }

and parse_assign_op env : Cst.assign_op =
  (Cst.Mul_assign
   <$ Parser.expect_eq StarEq
   <|> (Cst.Add_assign <$ Parser.expect_eq PlusEq)
   <|> (Cst.Sub_assign <$ Parser.expect_eq DashEq)
   <|> (Cst.Mod_assign <$ Parser.expect_eq PercentEq)
   <|> (Cst.Div_assign <$ Parser.expect_eq SlashEq)
   <|> (Cst.Bit_and_assign <$ Parser.expect_eq AmpersandEq)
   <|> (Cst.Bit_or_assign <$ Parser.expect_eq PipeEq)
   <|> (Cst.Bit_xor_assign <$ Parser.expect_eq CaretEq)
   <|> (Cst.Lshift_assign <$ Parser.expect_eq LangleLangleEq)
   <|> (Cst.Rshift_assign <$ Parser.expect_eq RangleRangleEq)
   <|> (Cst.Id_assign <$ Parser.expect_eq Eq))
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
  let expr = parse_ternary env in
  expr

and parse_ternary env : Cst.expr =
  let cond = parse_log_or env in
  ((fun env ->
     Parser.expect_eq Question env;
     let then_expr = parse_expr env in
     Parser.expect_eq Colon
     |> Parser.cut (Sexp [%message "expected colon in ternary"])
     |> Fn.( |> ) env;
     let else_expr = parse_expr env in
     Cst.Ternary { cond; then_expr; else_expr })
   <|> Parser.pure cond)
    env

and parse_log_or env : Cst.expr =
  chainl ~expr:parse_log_and ~op:(Cst.Log_or <$ Parser.expect_eq PipePipe) ~f:Cst.bin env

and parse_log_and env : Cst.expr =
  chainl
    ~expr:parse_bit_or
    ~op:(Cst.Log_and <$ Parser.expect_eq AmpersandAmpersand)
    ~f:Cst.bin
    env

and parse_bit_or env : Cst.expr =
  chainl ~expr:parse_bit_xor ~op:(Cst.Bit_or <$ Parser.expect_eq Pipe) ~f:Cst.bin env

and parse_bit_xor env : Cst.expr =
  chainl ~expr:parse_bit_and ~op:(Cst.Bit_xor <$ Parser.expect_eq Caret) ~f:Cst.bin env

and parse_bit_and env : Cst.expr =
  chainl
    ~expr:parse_equality
    ~op:(Cst.Bit_and <$ Parser.expect_eq Ampersand)
    ~f:Cst.bin
    env

and parse_equality env : Cst.expr =
  chainl
    ~expr:parse_comparison
    ~op:(Cst.Eq <$ Parser.expect_eq EqEq <|> (Cst.Neq <$ Parser.expect_eq BangEq))
    ~f:Cst.bin
    env

and parse_comparison env : Cst.expr =
  chainl
    ~expr:parse_shift
    ~op:
      (Cst.Lt
       <$ Parser.expect_eq Langle
       <|> (Cst.Gt <$ Parser.expect_eq Rangle)
       <|> (Cst.Le <$ Parser.expect_eq LangleEq)
       <|> (Cst.Ge <$ Parser.expect_eq RangleEq))
    ~f:Cst.bin
    env

and parse_shift env : Cst.expr =
  chainl
    ~expr:parse_add
    ~op:
      (Cst.Lshift
       <$ Parser.expect_eq LangleLangle
       <|> (Cst.Rshift <$ Parser.expect_eq RangleRangle))
    ~f:Cst.bin
    env

and parse_add env : Cst.expr =
  chainl
    ~expr:parse_mul
    ~op:(Cst.Add <$ Parser.expect_eq Plus <|> (Cst.Sub <$ Parser.expect_eq Dash))
    ~f:Cst.bin
    env

and parse_mul env : Cst.expr =
  chainl
    ~expr:parse_unary_expr
    ~op:
      (Cst.Mul
       <$ Parser.expect_eq Star
       <|> (Cst.Div <$ Parser.expect_eq Slash)
       <|> (Cst.Mod <$ Parser.expect_eq Percent))
    ~f:Cst.bin
    env

and parse_unary_expr env : Cst.expr =
  ((fun env ->
     Parser.expect_eq Dash env;
     let expr = parse_unary_expr env in
     Cst.Unary { op = Neg; expr })
   <|> (fun env ->
   Parser.expect_eq Tilde env;
   let expr = parse_unary_expr env in
   Cst.Unary { op = Bit_not; expr })
   <|> (fun env ->
   Parser.expect_eq Bang env;
   let expr = parse_unary_expr env in
   Cst.Unary { op = Log_not; expr })
   <|> parse_atom)
    env

and parse_atom env : Cst.expr =
  ((fun d -> Cst.Int_const d)
   <$> parse_num
   <|> parens parse_expr
   <|> (Parser.expect_eq True $> Cst.Bool_const true)
   <|> (Parser.expect_eq False $> Cst.Bool_const false)
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
