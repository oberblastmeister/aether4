open Shexp_process
open Shexp_process.Let_syntax
open Core

let ( / ) = Filename.concat

let run_test () =
  let%bind () = call [ "dune"; "test"; "./compiler/test" ] in
  return ()
;;

let run_filetest paths =
  let%bind () =
    if List.is_empty paths then fail (Failure "paths is emtpy") else return ()
  in
  let rec loop paths =
    match paths with
    | [] -> return ()
    | path :: paths ->
      let dir = Filename.dirname path in
      let base = Filename.basename path in
      let%bind () =
        call [ "dune"; "build"; "@@compiler/filetests/filetests" / dir / "run" / base ]
      in
      loop paths
  in
  loop paths
;;

let resolve_in_path prog =
  (* Do not try to resolve in the path if the program is something like
   * ./this.exe *)
  if String.split ~on:'/' prog |> List.length <> 1
  then Some prog
  else (
    let paths = Stdlib.Sys.getenv "PATH" |> String.split ~on:':' in
    List.map paths ~f:(fun d -> Stdlib.Filename.concat d prog)
    |> List.find ~f:Stdlib.Sys.file_exists)
;;

let resolve_in_path_exn prog =
  match resolve_in_path prog with
  | None -> failwith (Printf.sprintf "no program in path %s" prog)
  | Some prog -> prog
;;

let test_command =
  Command.basic
    ~summary:"Test"
    ~readme:(fun () -> "")
    (let open Command.Let_syntax in
     let open Command.Param in
     let open Cmd in
     return @@ fun () -> process "dune" [ "test" ] |> run)
;;

let filetest_command =
  Command.basic
    ~summary:"Filetest"
    ~readme:(fun () -> "")
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map paths = anon (sequence ("path" %: string)) in
     fun () -> eval (run_filetest paths))
;;

let run_check () =
  let%bind () = call [ "dune"; "build"; "@check" ] in
  return ()
;;

let check_command =
  Command.basic
    ~summary:"Check"
    ~readme:(fun () -> "")
    (let open Command.Let_syntax in
     let open Command.Param in
     return @@ fun () -> eval (run_check ()))
;;

let diff_command =
  Command.basic
    ~summary:"Diff"
    ~readme:(fun () -> "")
    (let open Command.Let_syntax in
     let open Command.Param in
     return
     @@ fun () ->
     eval
       Shexp_process.Let_syntax.(
         let%bind () = call [ "bash"; "-c"; "dune promotion diff" ] in
         return ()))
;;

let command =
  Command.group
    ~summary:"X task runner"
    [ "test", test_command
    ; "filetest", filetest_command
    ; "ft", filetest_command
    ; "c", check_command
    ; "diff", diff_command
    ]
;;

let () = Command_unix.run command
