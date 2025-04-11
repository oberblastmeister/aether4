include Std
include Aether4
include Ae_abs_x86_std
module Dominators = Ae_dominators
module Entity = Ae_entity_std
module Id_gen = Entity.Id_gen
module Ident = Entity.Ident

module Make_intern () = struct
  module Temp_intern = Entity.Intern.String_to_name.Make_global (Temp_entity.Witness) ()
  module Label_intern = Entity.Intern.String_to_name.Make_global (Label_entity.Witness) ()

  let temp = Temp_intern.intern
  let lab = Label_intern.intern
end
