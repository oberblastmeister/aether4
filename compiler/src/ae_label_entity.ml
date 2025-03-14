open struct
  module Entity = Ae_entity_std
end

include Entity.Make ()

module Dfs = Ae_data_graph_std.Dfs_gen.Make (struct
    open struct
      module Label = Ident
      module Table = Entity.Ident.Table
    end

    type t = unit Label.Table.t

    module Key = struct
      type t = Label.t
    end

    let create () = Table.create ()
    let add t key = Table.add_exn t ~key ~data:()
    let remove t k = Table.remove t k
    let mem t k = Table.mem t k
  end)
