open Std

type t =
  | RAX
  | RCX
  | RDX
  | RBX
  | RSP
  | RBP
  | RSI
  | RDI
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15
[@@deriving enum, equal, compare, sexp, hash, variants]

let scratch = R11

let%expect_test _ =
  let list =
    List.range min (max + 1) |> List.map ~f:(fun i -> of_enum i |> Option.value_exn)
  in
  print_s [%sexp (list : t list)];
  [%expect {| (RAX RCX RDX RBX RSP RBP RSI RDI R8 R9 R10 R11 R12 R13 R14 R15) |}]
;;
