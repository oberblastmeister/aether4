open Std

type t = String.Set.t [@@deriving sexp_of]

let empty = String.Set.empty
let declare_ident ident t = Set.remove t ident
let declare_ty_ident ident t = Set.add t ident
let is_ty_ident = Set.mem
