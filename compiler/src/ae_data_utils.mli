open Std

module Option_array : sig
  module OA := Option_array

  val resize_array : 'a OA.t -> int -> 'a OA.t
  val resize_for_index : 'a OA.t -> int -> 'a OA.t
end
