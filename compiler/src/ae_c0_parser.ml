open Std
module Token = Ae_c0_token
module Cst = Ae_c0_cst
module Span = Ae_span
module Spanned = Ae_spanned

module Stream_token = struct
  type t = Token.t Spanned.t [@@deriving sexp_of]

  let compare t1 t2 = Token.compare t1.Spanned.t t2.Spanned.t
  let equal t1 t2 = Token.equal t1.Spanned.t t2.Spanned.t
end

module Stream = Parsec.Make_stream (Stream_token)
open Ae_trace

module Error = struct
  type t = Sexp of Sexp.t [@@deriving sexp_of]
end

module Parser = Parsec.Make (struct
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

let parse_ty env : Cst.ty =
  (expect_eq_ Int $> Cst.Int <|> (expect_eq_ Bool $> Cst.Bool)) env
;;

let parse_ident env = Parser.expect (Spanned.map_option ~f:Token.ident_val) env

let parens p env =
  expect_eq_ LParen env;
  let res = p env in
  expect_eq_ RParen
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
     expect_eq_ LParen env;
     expect_eq_ RParen env;
     let block = parse_block env in
     ({ ty; name; block } : Cst.program))
   |> Parser.cut (Sexp [%message "invalid program"]))
    env

and parse_block env : Cst.block =
  expect_eq_ LBrace env;
  let stmts = Parser.many parse_stmt env in
  expect_eq_ RBrace
  |> Parser.cut
       (Sexp
          [%message
            "expected closing brace for block"
              (Stream.peek (Parser.stream env) : Token.t Spanned.t option)])
  |> Fn.( |> ) env;
  { stmts }

and parse_stmt env : Cst.stmt =
  let stmt =
    (parse_if
     <|> parse_for
     <|> parse_while
     <|> ((fun b -> Cst.Block b) <$> parse_block)
     <|> (parse_semi_stmt <* expect_eq_ Semi))
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
  expect_eq_ If env;
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
         expect_eq_ Else env;
         parse_stmt |> Parser.cut (Sexp [%message "expected if stmt"]) |> Fn.( |> ) env)
      env
  in
  Cst.If { cond; body1; body2 }

and parse_while env : Cst.stmt =
  expect_eq_ While env;
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
  expect_eq_ For env;
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
  expect_eq_ Semi env;
  let cond = parse_expr env in
  expect_eq_ Semi env;
  let incr = (Parser.optional parse_semi_stmt) env in
  { init; cond; incr }

and parse_return env : Cst.expr =
  expect_eq_ Return env;
  parse_expr env

and parse_decl env : Cst.decl =
  let ty = parse_ty env in
  let name = parse_ident env in
  let expr =
    Parser.optional
      (fun env ->
         expect_eq_ Eq env;
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
  (Cst.Incr <$ expect_eq_ PlusPlus <|> (Cst.Decr <$ expect_eq_ DashDash)) env

and parse_assign env : Cst.assign =
  let lvalue = parse_lvalue env in
  let op = parse_assign_op env in
  let expr = parse_expr env in
  { lvalue; op; expr }

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

and parse_lvalue env : Cst.lvalue =
  ((fun env ->
     expect_eq_ LParen env;
     let lvalue = parse_lvalue env in
     expect_eq_ RParen env;
     lvalue)
   <|> parse_ident)
    env

and parse_expr env : Cst.expr =
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
   let bang = expect_eq Bang env in
   let expr = parse_unary_expr env in
   Cst.Unary { op = Log_not; expr; span = bang.span ++ Cst.expr_span expr })
   <|> parse_atom)
    env

and parse_atom env : Cst.expr =
  ((fun d -> Cst.Int_const d)
   <$> parse_num
   <|> parens parse_expr
   <|> (Cst.bool_const <$> parse_true)
   <|> (Cst.bool_const <$> parse_false)
   <|> (Cst.var <$> parse_ident))
    env

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

let parse tokens =
  let stream = Stream.of_chunk tokens in
  Parser.with_env stream (fun env -> parse_program env)
  |> Parsec.Parse_result.to_result_exn
  |> Result.map_error ~f:(fun (Sexp s) -> Core.Error.create_s s)
;;
