module Entity = Ae_entity_public
module Id = Entity.Id
module Table = Entity.Id.Table
open Table.Syntax

module type Arg = sig
  module Witness : Entity.Witness.S

  module Data : sig
    type t

    val next : t -> Witness.t Id.t option
    val set_next : t -> Witness.t Id.t option -> t
    val prev : t -> Witness.t Id.t option
    val set_prev : t -> Witness.t Id.t option -> t
  end
end

module type S = sig
  module Arg : Arg
  open Arg

  type map = (Witness.t, Data.t) Id.Map.t

  val remove : map -> Witness.t Id.t -> unit
  val insert_after : after:Witness.t Id.t -> map -> Witness.t Id.t
end

module Make (Arg : Arg) = struct
  open Arg

  type map = (Witness.t, Data.t) Id.Map.t

  let remove map node =
    let next = Data.next map.!(node) in
    let prev = Data.prev map.!(node) in
    (match next with
     | None -> ()
     | Some next ->
       map.!(next) <- Data.set_prev map.!(next) prev;
       map.!(node) <- Data.set_next map.!(node) None);
    (match prev with
     | None -> ()
     | Some prev ->
       map.!(prev) <- Data.set_next map.!(prev) next;
       map.!(node) <- Data.set_prev map.!(node) None);
    ()
  ;;

  let insert_after ~after:node map insert =
    let next = Data.next map.!(node) in
    map.!(insert) <- Data.set_prev map.!(insert) (Some node);
    map.!(node) <- Data.set_next map.!(node) (Some insert);
    map.!(insert) <- Data.set_next map.!(insert) next;
    (match next with
     | None -> ()
     | Some next ->
       map.!(next) <- Data.set_prev map.!(next) (Some insert);
       ());
    ()
  ;;
end
