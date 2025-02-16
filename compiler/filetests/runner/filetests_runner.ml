open Std

let () =
  let path = (Sys.get_argv ()).(1) in
  let contents = In_channel.read_all path in
  (* printf "contents: %s\n" contents; *)
  Out_channel.print_string contents;
  ()
;;
