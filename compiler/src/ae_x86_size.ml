type t =
  | Byte
  | Word
  | Dword
  | Qword
[@@deriving sexp_of, equal, compare]

let to_bytes = function
  | Byte -> 1
  | Word -> 2
  | Dword -> 4
  | Qword -> 8
;;

let to_bits = function
  | Byte -> 8
  | Word -> 16
  | Dword -> 32
  | Qword -> 64
;;
