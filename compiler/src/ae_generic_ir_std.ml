open Std
module Sigs = Ae_generic_ir_sigs
module Make_ir = Ae_generic_ir_make.Make_ir
module Liveness = Ae_generic_ir_liveness
module Check_ssa = Ae_generic_ir_check_ssa
module Split_critical = Ae_generic_ir_split_critical
module Normalize_block_params = Ae_generic_ir_normalize_block_params
module Destruct_ssa = Ae_generic_ir_destruct_ssa
module Check_well_formed = Ae_generic_ir_check_well_formed
module Check_types = Ae_generic_ir_check_types
module Convert_ssa = Ae_generic_ir_convert_ssa

(* module Make_all (Arg : Sigs.Arg) = struct
  module Ir = struct
    include Make_ir (Arg)
  end

  module Liveness = Liveness.Make (Ir)
  module Check_ssa = Check_ssa.Make (Ir)
  module Split_critical = Split_critical.Make (Ir)
  module Normalize_block_params = Normalize_block_params.Make (Ir)
  module Check_well_formed = Check_well_formed.Make (Ir)
  module Check_types = Check_types.Make (Ir)
  module Convert_ssa = Convert_ssa.Make (Ir)

end

module Make_all_S (Arg : Sigs.Arg) = struct
  module type S = module type of struct
    include Make_all (Arg)
  end
end *)
