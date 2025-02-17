open Std
open Aether4.Ae_std
module Stdenv = Eio.Stdenv
module Path = Eio.Path
module Flow = Eio.Flow
module Process = Eio.Process
module Buf_read = Eio.Buf_read

module Test = struct
  module Options = struct
    module Kind = struct
      type t =
        | CompileAndRun of { option : unit option [@sexp.option] }
        | CompileFail of { option : unit option [@sexp.option] }
      [@@deriving sexp]
    end

    type t = Test of Kind.t [@@deriving sexp]

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
  eprintf "running filetests: %s\n" path;
  let open Result.Let_syntax in
  let@ env = Eio_main.run in
  let contents =
    let@ file = Path.with_open_in Path.(Stdenv.fs env / path) in
    Flow.read_all file
  in
  let test = Test.parse contents in
  let res = Driver.compile_source_to_a_out env test.source Path.(Stdenv.fs env / path) in
  let%bind () =
    match res, test.options with
    | Ok (a_out_path, _changed), Test (CompileAndRun _) ->
      let proc = Stdenv.process_mgr env in
      let output =
        Process.parse_out proc Buf_read.take_all [ Path.native_exn a_out_path ]
      in
      { test with expect = output } |> Test.format |> Out_channel.print_string;
      Ok ()
    | Error e, Test (CompileFail _) ->
      { test with expect = Error.to_string_hum e }
      |> Test.format
      |> Out_channel.print_string;
      Ok ()
    | Ok _, Test (CompileFail _) ->
      error_s [%message "Expected test to fail to compile, but test compiled properly"]
    | Error _, Test (CompileAndRun _) -> error_s [%message "Could not compile test"]
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
