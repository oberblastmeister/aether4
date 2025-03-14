open Std
open Aether4
open Aether4.Ae_std
module Stdenv = Eio.Stdenv
module Path = Eio.Path
module Flow = Eio.Flow
module Process = Eio.Process
module Buf_read = Eio.Buf_read
module Test_options = Filetests_test_options
module Trace = Ae_trace
module Chaos_mode = Ae_chaos_mode

module Test = struct
  type t =
    { options : Test_options.t
    ; source : string
    ; expect : string
    }

  let parse contents =
    let parts = String.lsplit2_on contents ~on:"----" in
    let source, expect = Option.value_map ~f:Fn.id ~default:(contents, "") parts in
    let options = Test_options.parse source in
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

let run_test env path =
  eprintf "filetest %s\n" (Filename_unix.realpath path);
  let open Result.Let_syntax in
  let contents =
    let@ file = Path.with_open_in Path.(Stdenv.fs env / path) in
    Flow.read_all file
  in
  let test = Test.parse contents in
  let@ () = Trace.with_trace test.options.trace in
  let@ () =
    Chaos_mode.with_options (fun options ->
      { options with spill_mode = test.options.chaos_spill_mode })
  in
  (* make sure to print the source first *)
  (* this makes sure that any later stuff gets printed after and gets formatted properly *)
  Test.format_source_with_bar test |> print_string;
  let emit = test.options.emit in
  let env = Driver.Env.create ~emit env Path.(Stdenv.fs env / path) in
  let res =
    try Driver.compile_source_to_a_out env test.source with
    | exn ->
      print_endline (Exn.to_string exn);
      print_endline (Printexc.get_backtrace ());
      error_s [%message "Compiler panicked!"]
  in
  let%bind () =
    match res, test.options.kind with
    | Ok (a_out_path, _changed), CompileAndRun { status = expected_status } ->
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
                (status : Test_options.Exit_status.t)
                (expected_status : Test_options.Exit_status.t)]
        | None, `Exited 0 -> Ok ()
        | None, _ ->
          error_s
            [%message
              "Process did not exit normally" (status : Test_options.Exit_status.t)]
        | _ -> Ok ()
      in
      let output = Buffer.contents buf in
      print_string output;
      Ok ()
    | Error e, CompileFail _ ->
      Error.to_string_hum e |> print_string;
      Ok ()
    | Ok _, CompileFail _ ->
      error_s [%message "Expected test to fail to compile, but test compiled properly"]
    | Error e, CompileAndRun _ ->
      Error.tag_s e ~tag:[%message "Could not compile test"] |> Error
  in
  Ok ()
;;

let command =
  Command.basic
    ~summary:"Filetests runner"
    ~readme:(fun () -> "More detailed information")
    begin
      let open Command.Let_syntax in
      let open Command.Param in
      let%map path = anon ("filename" %: Filename_unix.arg_type) in
      fun () ->
        let@ env = Eio_main.run in
        match run_test env path with
        | Ok () -> ()
        | Error e ->
          print_endline
          @@ Error.to_string_hum (Error.tag_s e ~tag:[%message "Filetest failed"]);
          ()
    end
;;

let () = Command_unix.run command
