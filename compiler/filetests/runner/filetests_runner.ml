open Std
open Aether4.Ae_std
module Stdenv = Eio.Stdenv
module Path = Eio.Path
module Flow = Eio.Flow
module Process = Eio.Process
module Buf_read = Eio.Buf_read
module Options = Filetests_options

let add_output contents output =
  let code_part =
    let parts = String.split_on contents ~on:"----" in
    List.hd parts |> Option.value ~default:contents
  in
  let expect_output =
    code_part
    ^ (if
         String.length code_part > 0
         && Char.equal (String.get code_part (String.length code_part - 1)) '\n'
       then ""
       else "\n")
    ^ "----\n"
    ^ output
  in
  expect_output
;;

let run_test path =
  eprintf "running filetests: %s\n" path;
  let open Result.Let_syntax in
  let@ env = Eio_main.run in
  let contents =
    let@ file = Path.with_open_in Path.(Stdenv.fs env / path) in
    Flow.read_all file
  in
  let options = Options.parse_options contents in
  let res = Driver.compile_path_to_a_out env Path.(Stdenv.fs env / path) in
  let%bind () =
    match res, options with
    | Ok (a_out_path, _changed), Test (CompileAndRun _) ->
      let proc = Stdenv.process_mgr env in
      let output =
        Process.parse_out proc Buf_read.take_all [ Path.native_exn a_out_path ]
      in
      let expect_output = add_output contents output in
      Out_channel.print_string expect_output;
      Ok ()
    | Error e, Test (CompileFail _) ->
      let expect_output = add_output contents (Error.to_string_hum e) in
      Out_channel.print_string expect_output;
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
