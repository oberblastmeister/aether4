open Std

module String_mutable_map = struct
  module Key = String
  include String.Table

  let create = String.Table.create
  let find = Hashtbl.find
  let set = Hashtbl.set
  let remove = Hashtbl.remove
  let mem = Hashtbl.mem
end

module String_mutable_set = struct
  module Key = String
  include String.Hash_set

  let create = String.Hash_set.create
  let add = Hash_set.add
  let remove = Hash_set.remove
  let mem = Hash_set.mem
end
