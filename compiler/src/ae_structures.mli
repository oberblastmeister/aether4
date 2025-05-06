open Std
module Signatures := Ae_signatures
module String_mutable_map : Signatures.Mutable_map with module Key = String
module String_mutable_set : Signatures.Mutable_set with module Key = String
