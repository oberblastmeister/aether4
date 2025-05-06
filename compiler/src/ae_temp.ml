open struct
  module Ident = Ae_ident
end

include Ident.Make_with_name_info ()
module Dfs = Ae_data_graph_std.Dfs_gen.Make (Mutable_map) (Mutable_set)
