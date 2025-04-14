open Std

module T = struct
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
  [@@deriving enum, equal, compare, sexp, hash, variants, string]
end

include Base.Comparable.Make (T)
include T

let of_enum_exn i = of_enum i |> Option.value_exn
let scratch = R11
let num = max - min + 1

let%expect_test _ =
  print_s [%message (min : int) (max : int) (num : int)];
  [%expect {| ((min 0) (max 15) (num 16)) |}]
;;

let%expect_test _ =
  let list =
    List.range min (max + 1) |> List.map ~f:(fun i -> of_enum i |> Option.value_exn)
  in
  print_s [%sexp (list : t list)];
  [%expect {| (RAX RCX RDX RBX RSP RBP RSI RDI R8 R9 R10 R11 R12 R13 R14 R15) |}]
;;
