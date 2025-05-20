open Std

open struct
  module Ty = Ae_x86_ty
end

module Make (Temp : sig
    type t [@@deriving sexp_of, equal]
  end) =
struct
  module Index = struct
    type t =
      { index : Temp.t
      ; scale : int
      }
    [@@deriving sexp_of, equal]

    let create index scale = { index; scale }
    let iter_uses { index; scale = _ } ~f = f index
    let map_uses { index; scale } ~f = { index = f index; scale }
  end

  type t =
    { (* None means RIP relative *)
      base : Temp.t option
    ; index : Index.t option
    ; offset : [ `Int of int | `Label of string ]
    }
  [@@deriving sexp_of, equal]

  let create ?index ?(offset = `Int 0) base = { base = Some base; index; offset }
  let create_rip_relative ?index offset = { base = None; index; offset }

  let iter_uses_with_ty { base; index; offset = _ } ~f =
    Option.iter base ~f:(fun temp -> f (temp, Ty.Qword));
    (Option.iter @> Index.iter_uses) index ~f:(fun temp -> f (temp, Ty.Qword));
    ()
  ;;

  let map_uses { base; index; offset } ~f =
    let base = Option.map ~f base in
    let index = (Option.map & Index.map_uses) index ~f in
    { base; index; offset }
  ;;

  let iter_uses t ~f = iter_uses_with_ty t |> Iter.map ~f:fst |> Iter.iter ~f
end
