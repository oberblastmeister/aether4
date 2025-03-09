type t =
  | Byte
  | Word
  | Dword
  | Qword
[@@deriving sexp_of, equal, compare]

let size_to_bits = function
  | Byte -> 8
  | Word -> 16
  | Dword -> 32
  | Qword -> 64
;;
