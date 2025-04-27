open Std

open struct
  module Path = Ae_path
end

let copy ~src ~dst =
  let data = In_channel.read_all src in
  let@: out_chan = Out_channel.with_file dst ~perm:0o777 in
  Out_channel.output_string out_chan data
;;

let create_dir ?perm path = Core_unix.mkdir ?perm path
let rec create_dir_all ?perm path = Core_unix.mkdir_p ?perm path
