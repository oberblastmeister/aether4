open Std

type t =
  | Ident of string
  | Hexnum of string
  | Decnum of string
  | Semi
  | Extern
  | Typedef
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
  | Comma
  | PlusPlus
  | DashDash
  | Langle
  | Rangle
  | LangleLangle
  | RangleRangle
  | LangleEq
  | RangleEq
  | AmpersandEq
  | PipeEq
  | CaretEq
  | LangleLangleEq
  | RangleRangleEq
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
  | Void
  | Unknown of string
  | Assert
  | Eof
[@@deriving sexp_of, equal, compare, variants]
