open Std

let ( @> ) = Fold.( @> )

module Index = struct
  type 'r t =
    { index : 'r
    ; scale : int
    }
  [@@deriving sexp_of]

  let create index scale = { index; scale }
  let iter_regs { index; scale = _ } ~f = f index
end

module Base = struct
  type 'r t =
    | Reg of 'r
    | Rsp
  [@@deriving sexp_of, variants]

  let iter_regs base ~f =
    match base with
    | Reg r -> f r
    | Rsp -> ()
  ;;
end

type 'r t =
  { base : 'r Base.t
  ; index : 'r Index.t option
  ; offset : int
  }
[@@deriving sexp_of]

let create base ?index offset = { base = Base.reg base; index; offset }

let iter_regs { base; index; offset = _ } ~f =
  Base.iter_regs base ~f;
  (Option.iter @> Index.iter_regs) index ~f;
  ()
;;
