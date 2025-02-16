open Std

let generate_rules dir base =
  print_endline
    [%string
      {|
(rule
 (with-stdout-to %{base}.c0.output
  (run %{"%{"}bin:filetests_runner} %{dir}/%{base}.c0)
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

let () =
  let args = Sys.get_argv () in
  let dir = args.(1) in
  let paths = Sys_unix.readdir dir in
  Array.sort ~compare:String.compare paths;
  paths
  |> Array.filter_map ~f:(Filename.chop_suffix_opt ~suffix:".c0")
  |> Array.iter ~f:(generate_rules dir);
  ()
;;
