open struct
  module Ident = Ae_ident
end

include Ident.Make_with_name_info ()

module Dfs = Ae_data_graph_std.Dfs_gen.Make (struct
    module Key = struct
      type nonrec t = t
    end

    type t = unit Table.t

    let create () = Table.create ()
    let add t key = Table.add_exn t ~key ~data:()
    let remove t k = Table.remove t k
    let mem t k = Table.mem t k
  end)
