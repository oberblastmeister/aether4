open Std
open Aether4
open Ae_std
module Chaos_mode = Ae_chaos_mode

(* module Exit_status = struct
  type t =
    [ `Exited of int
      (*
         Process exited with the given return code.
      *)
    | `Signaled of int
      (*
         Process was killed by the given signal.
      *)
    ]

  let sexp_of_t (t : t) =
    match t with
    | `Exited i -> [%sexp (("Exited", i) : string * int)]
    | `Signaled i ->
      [%sexp (("Signaled", Fmt.str "%a" Fmt.Dump.signal i) : string * string)]
  ;;

  let t_of_sexp (s : Sexp.t) =
    match s with
    | List [ Atom "Signaled"; Atom s ] ->
      (match s with
       | "SIGFPE" -> `Signaled Caml_sys.sigfpe
       | "SIGSEGV" -> `Signaled Caml_sys.sigsegv
       | "SIGABRT" -> `Signaled Caml_sys.sigabrt
       | _ -> raise_s [%message "Failed to parse signal for exit status"])
    | List [ Atom "Exited"; s ] -> `Exited (Int.t_of_sexp s)
    | _ -> raise_s [%message "Could not parse sexp into exit status"]
  ;;
end *)

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
