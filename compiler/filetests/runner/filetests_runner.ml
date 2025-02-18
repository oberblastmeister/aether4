open Std
open Aether4.Ae_std
module Stdenv = Eio.Stdenv
module Path = Eio.Path
module Flow = Eio.Flow
module Process = Eio.Process
module Buf_read = Eio.Buf_read

module Test = struct
  module Options = struct
    module Exit_status = struct
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
           | _ -> raise_s [%message "Failed to parse signal for exit status"])
        | List [ Atom "Exited"; s ] -> `Exited (Int.t_of_sexp s)
        | _ -> raise_s [%message "Could not parse sexp into exit status"]
      ;;
    end

    module Kind = struct
      type t =
        | CompileAndRun of
            { emit : Driver.Emit.t list [@sexp.list]
            ; status : Exit_status.t option [@sexp.option]
            }
        | CompileFail of { emit : Driver.Emit.t list [@sexp.list] }
      [@@deriving sexp]

      let get_emit (CompileAndRun { emit; status = _ } | CompileFail { emit }) = emit
    end

    type t = Test of Kind.t [@@deriving sexp]

    let get_kind (Test kind) = kind

    let parse source =
      let _, source = String.lsplit2_on_exn source ~on:"/*" in
      let source, _ = String.lsplit2_on_exn source ~on:"*/" in
      let test = t_of_sexp (Sexp.of_string source) in
      test
    ;;
  end

  type t =
    { options : Options.t
    ; source : string
    ; expect : string
    }

  let parse contents =
    let parts = String.lsplit2_on contents ~on:"----" in
    let source, expect = Option.value_map ~f:Fn.id ~default:(contents, "") parts in
    let options = Options.parse source in
    { options; source; expect }
  ;;

  let format_source_with_bar { source; expect = _; options = _ } =
    source
    ^ (if
         String.length source > 0
         && Char.equal (String.get source (String.length source - 1)) '\n'
       then ""
       else "\n")
    ^ "----\n"
  ;;

  let format { source; expect; options = _ } =
    source
    ^ (if
         String.length source > 0
         && Char.equal (String.get source (String.length source - 1)) '\n'
       then ""
       else "\n")
    ^ "----\n"
    ^ expect
  ;;
end

let run_test path =
  eprintf "filetest %s\n" (Filename_unix.realpath path);
  let open Result.Let_syntax in
  let@ env = Eio_main.run in
  let contents =
    let@ file = Path.with_open_in Path.(Stdenv.fs env / path) in
    Flow.read_all file
  in
  let test = Test.parse contents in
  (* make sure to print the source first *)
  (* this makes sure that any later stuff gets printed after and gets formatted properly *)
  Test.format_source_with_bar test |> print_string;
  let emit = Test.Options.Kind.get_emit @@ Test.Options.get_kind test.options in
  let env = Driver.Env.create ~emit env Path.(Stdenv.fs env / path) in
  let res = Driver.compile_source_to_a_out env test.source in
  let%bind () =
    match res, test.options with
    | ( Ok (a_out_path, _changed)
      , Test (CompileAndRun { emit = _; status = expected_status }) ) ->
      let pmgr = Stdenv.process_mgr env.env in
      let@ sw = Eio.Switch.run ~name:"proc" in
      let buf = Buffer.create 100 in
      let stdout_sink = Flow.buffer_sink buf in
      let stderr_sink = Flow.buffer_sink buf in
      let proc =
        Process.spawn
          ~stdout:stdout_sink
          ~stderr:stderr_sink
          ~executable:(Path.native_exn a_out_path)
          ~sw
          pmgr
          []
      in
      let status = Process.await proc in
      let%bind () =
        match expected_status, status with
        | Some expected_status, status when not (Poly.equal expected_status status) ->
          error_s
            [%message
              "The exit status was not equal"
                (status : Test.Options.Exit_status.t)
                (expected_status : Test.Options.Exit_status.t)]
        | None, `Exited 0 -> Ok ()
        | None, _ ->
          error_s
            [%message
              "Process did not exit normally" (status : Test.Options.Exit_status.t)]
        | _ -> Ok ()
      in
      let output = Buffer.contents buf in
      print_string output;
      Ok ()
    | Error e, Test (CompileFail _) ->
      Error.to_string_hum e |> print_string;
      Ok ()
    | Ok _, Test (CompileFail _) ->
      error_s [%message "Expected test to fail to compile, but test compiled properly"]
    | Error e, Test (CompileAndRun _) ->
      Error.tag_s e ~tag:[%message "Could not compile test"] |> Error
  in
  Ok ()
;;

let command =
  Command.basic_or_error
    ~summary:"Filetests runner"
    ~readme:(fun () -> "More detailed information")
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map path = anon ("filename" %: Filename_unix.arg_type) in
     fun () -> run_test path)
;;

let () = Command_unix.run command
