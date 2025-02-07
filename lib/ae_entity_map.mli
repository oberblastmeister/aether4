open Std
module Signatures := Ae_signatures
module Intf := Ae_entity_map_intf

module type Key = Intf.Key
module type S = Intf.S

module Make (Key : Key) : S with module Key = Key
