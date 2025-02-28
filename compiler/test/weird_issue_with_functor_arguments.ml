module type T = sig
  type t
end

module Make (T : T) : T = struct
  type t
end

module type Arg = sig
  module T : T

  type t

  val f : t -> Make(T).t
end

module type S = sig
  module Arg : Arg
  open Arg

  val f : t -> Make(T).t
end

(* module Make2 (Arg : Arg) : S with module Arg = Arg = struct
  open Arg
  module Arg = Arg

  let f = f
end *)
