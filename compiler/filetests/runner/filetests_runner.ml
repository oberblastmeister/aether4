open Std

let () =
  let path = (Sys.get_argv ()).(1) in
  let contents = In_channel.read_all path in
  Out_channel.print_string contents;
  printf "bruh\n";
  ()
;;
