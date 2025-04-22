include Std
include Aether4
include Ae_abs_x86_std
module Dominators = Ae_dominators

module Make_intern () = struct
  module Temp_intern = Temp.Intern.Make_global ()
  module Label_intern = Label.Intern.Make_global ()

  let temp = Temp_intern.intern
  let lab = Label_intern.intern
end
