open Std

type t =
  | Ident of string
  | Hexnum of string
  | Decnum of string
  | Semi
  | Plus
  | Eq
  | Star
  | Dash
  | Slash
  | Percent
  | PlusEq
  | DashEq
  | StarEq
  | SlashEq
  | PercentEq
  | LParen
  | RParen
  | LBrace
  | RBrace
  | Return
  | Int
  | If
  | Else
  | While
  | For
  | Unknown of string
  | Eof
[@@deriving sexp_of, equal, compare, variants]
