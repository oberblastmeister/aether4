open Core

let generate_rules_diff dir base =
  let v s = "%{" ^ s ^ "}" in
  print_endline
    [%string
      {|
(rule
 (deps
  %{dir}/%{base}.c0
  %{v "project_root"}/compiler/filetests/runner/filetests_runner.exe
  ; filetests_runner.exe needs libc0_runtime.a so it can link it with the compiled c0 program
  %{v "project_root"}/compiler/filetests/runner/libc0_runtime.a
 )
 (action
  (with-stdout-to %{base}.c0.output
    (run 
     %{v "project_root"}/compiler/filetests/runner/filetests_runner.exe
     %{dir}/%{base}.c0
    )
  )
 )
)

(rule
 (aliases runtest %{base})
 (action
  (diff %{dir}/%{base}.c0 %{base}.c0.output)
 )
)
    |}]
;;

let generate_rules_run dir base =
  let v s = "%{" ^ s ^ "}" in
  print_endline
    [%string
      {|
(rule
 (aliases runtest %{base})
 (deps
  %{dir}/%{base}.c0
  %{v "project_root"}/compiler/filetests/runner/filetests_runner.exe
  ; filetests_runner.exe needs libc0_runtime.a so it can link it with the compiled c0 program
  %{v "project_root"}/compiler/filetests/runner/libc0_runtime.a
 )
 (action
  (run 
   %{v "project_root"}/compiler/filetests/runner/filetests_runner.exe
   %{dir}/%{base}.c0
  )
 )
)
|}]
;;

let () =
  let args = Sys.get_argv () in
  let dir = args.(1) in
  let paths = Sys_unix.readdir dir in
  Array.sort ~compare:String.compare paths;
  paths
  |> Array.filter_map ~f:(Filename.chop_suffix_opt ~suffix:".c0")
  |> Array.iter ~f:(generate_rules_diff dir);
  ()
;;
