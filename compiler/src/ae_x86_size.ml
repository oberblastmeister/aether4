type t =
  | Byte
  | Word
  | Dword
  | Qword
[@@deriving sexp_of, equal, compare]
