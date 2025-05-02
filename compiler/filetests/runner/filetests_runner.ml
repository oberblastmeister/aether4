open Std
open Aether4
open Aether4.Ae_std
module Process = Ae_process
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
    ^ (if String.length source > 0 && Char.equal source.[String.length source - 1] '\n'
       then ""
       else "\n")
    ^ "----\n"
  ;;

  let format { source; expect; options = _ } =
    source
    ^ (if String.length source > 0 && Char.equal source.[String.length source - 1] '\n'
       then ""
       else "\n")
    ^ "----\n"
    ^ expect
  ;;
end

let run_test path =
  eprintf "filetest %s\n" (Filename_unix.realpath path);
  let open Result.Let_syntax in
  let contents = In_channel.read_all path in
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
  let env = Driver.Env.create ~emit path in
  let res =
    try
      Driver.compile_source_to_a_out
        env
        [ test.source; Filename.concat (Driver.find_runtime_dir ()) "libc0_test_utils.a" ]
    with
    | exn ->
      print_endline (Exn.to_string exn);
      print_endline (Printexc.get_backtrace ());
      error_s [%message "Compiler panicked!"]
  in
  let%bind () =
    match res, test.options.kind with
    | Ok a_out_path, CompileAndRun { status = expected_status } ->
      let fd_read, fd_write = Spawn.safe_pipe () in
      let status =
        try
          let proc =
            Core_unix.create_process_with_fds
              ~prog:a_out_path
              ~args:[]
              ~env:(`Extend [])
              ~stdin:Generate
              ~stdout:(Use_this fd_write)
              ~stderr:(Use_this fd_write)
              ()
          in
          (* This gets duped in the child, we need to close this so that input_all receives EOF *)
          Core_unix.close fd_write;
          Core_unix.close proc.stdin;
          let status = Core_unix.waitpid proc.pid in
          let in_channel = Core_unix.in_channel_of_descr fd_read in
          let output = in_channel |> In_channel.input_all in
          print_string output;
          In_channel.close in_channel;
          status
        with
        | exn ->
          print_string "exception when running process\n";
          Core_unix.close fd_read;
          Core_unix.close fd_write;
          raise exn
      in
      let%bind () =
        match expected_status, status with
        | Some expected_status, Error status when not (Poly.equal expected_status status)
          ->
          error_s
            [%message
              "The exit status was not equal"
                (status : Core_unix.Exit_or_signal.error)
                (expected_status : Core_unix.Exit_or_signal.error)]
        | None, Ok () -> Ok ()
        | None, Error status ->
          error_s
            [%message
              "Process did not exit normally" (status : Core_unix.Exit_or_signal.error)]
        | _ -> Ok ()
      in
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
        match run_test path with
        | Ok () -> ()
        | Error e ->
          printf
            "%s\n"
            (Error.to_string_hum (Error.tag_s e ~tag:[%message "Filetest failed"]));
          ()
    end
;;

let () = Command_unix.run command
