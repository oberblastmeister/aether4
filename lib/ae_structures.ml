open Std

module String_mutable_map = struct
  module Key = String
  include String.Table

  let create () = String.Table.create ()
  let find = Hashtbl.find
  let set = Hashtbl.set
  let remove = Hashtbl.remove
  let mem = Hashtbl.mem
end
