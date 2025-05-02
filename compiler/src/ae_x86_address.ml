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

  module Base = struct
    type t = Reg of Temp.t [@@deriving sexp_of, variants, equal]

    let map_uses t ~f =
      match t with
      | Reg r -> Reg (f r)
    ;;

    let iter_temps base ~f =
      match base with
      | Reg r -> f r
    ;;
  end

  type t =
    { base : Base.t
    ; index : Index.t option
    ; offset : int
    }
  [@@deriving sexp_of, equal]

  let create ?index ?(offset = 0) base = { base = Base.reg base; index; offset }

  let iter_uses_with_ty { base; index; offset = _ } ~f =
    Base.iter_temps base ~f:(fun temp -> f (temp, Ty.Qword));
    (Option.iter @> Index.iter_uses) index ~f:(fun temp -> f (temp, Ty.Qword));
    ()
  ;;

  let map_uses { base; index; offset } ~f =
    let base = Base.map_uses base ~f in
    let index = (Option.map & Index.map_uses) index ~f in
    { base; index; offset }
  ;;

  let iter_uses t ~f = iter_uses_with_ty t |> Iter.map ~f:fst |> Iter.iter ~f
end
