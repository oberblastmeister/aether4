open Std

type t =
  | Ident of string
  | Hexnum of string
  | Decnum of string
  | Semi
  | Colon
  | Plus
  | Question
  | Eq
  | Bang
  | EqEq
  | BangEq
  | Star
  | Dash
  | Slash
  | Percent
  | PlusEq
  | DashEq
  | StarEq
  | SlashEq
  | PercentEq
  | PlusPlus
  | DashDash
  | Langle
  | Rangle
  | LangleLangle
  | RangleRangle
  | LangleEq
  | RangleEq
  | LParen
  | RParen
  | LBrace
  | RBrace
  | Return
  | Tilde
  | Caret
  | Ampersand
  | Pipe
  | PipePipe
  | AmpersandAmpersand
  | Int
  | Bool
  | True
  | False
  | If
  | Else
  | While
  | For
  | Unknown of string
  | Eof
[@@deriving sexp_of, equal, compare, variants]
