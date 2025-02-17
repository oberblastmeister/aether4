open Std

module Kind = struct
  type t =
    | CompileAndRun of { option : unit option [@sexp.option] }
    | CompileFail of { option : unit option [@sexp.option] }
  [@@deriving sexp]
end

module Test = struct
  type t = Test of Kind.t [@@deriving sexp]
end

let parse_options source =
  let _, source = String.lsplit2_on_exn source ~on:"/*" in
  let source, _ = String.lsplit2_on_exn source ~on:"*/" in
  let test = Test.t_of_sexp (Sexp.of_string source) in
  test
;;
