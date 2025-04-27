open Std
open Aether4
open Ae_std
module Chaos_mode = Ae_chaos_mode

module Kind = struct
  type t =
    | CompileAndRun of { status : Core_unix.Exit_or_signal.error option [@sexp.option] }
    | CompileFail of { option : unit option [@sexp.option] }
  [@@deriving sexp]
end

type t =
  { kind : Kind.t
  ; status : Core_unix.Exit_or_signal.error option [@sexp.option]
  ; trace : bool [@sexp.bool]
  ; emit : Driver.Emit.t list [@sexp.list]
  ; chaos_spill_mode : Chaos_mode.Spill_mode.t option [@sexp.option]
  }
[@@deriving sexp]

let parse source =
  let _, source = String.lsplit2_on_exn source ~on:"/*" in
  let source, _ = String.lsplit2_on_exn source ~on:"*/" in
  let test = t_of_sexp (Sexp.of_string source) in
  test
;;
