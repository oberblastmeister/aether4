module Sigs = Ae_generic_ir_sigs
module Make_ir = Ae_generic_ir_make.Make_ir
module Liveness = Ae_generic_ir_liveness
module Check_ssa = Ae_generic_ir_check_ssa
module Split_critical = Ae_generic_ir_split_critical
module Normalize_block_params = Ae_generic_ir_normalize_block_params
module Destruct_ssa = Ae_generic_ir_destruct_ssa

module Make_all (Arg : Sigs.Arg) = struct
  module Ir = Make_ir (Arg)
  module Liveness = Liveness.Make (Ir)
  module Check_ssa = Check_ssa.Make (Ir)
  module Split_critical = Split_critical.Make (Ir)
  module Normalize_block_params = Normalize_block_params.Make (Ir)
end
