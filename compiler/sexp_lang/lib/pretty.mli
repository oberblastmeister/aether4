open O

type ann =
  | Line
  | IndentLine
[@@deriving equal, compare, sexp]

module Delim : sig
  type t =
    | Paren
    | Brack
    | Brace
  [@@deriving sexp, equal, compare, sexp]
end

type t =
  | List of t list * Delim.t
  | Atom of string
  | Keyword of string
  | Ann of ann
[@@deriving equal, compare, sexp]
