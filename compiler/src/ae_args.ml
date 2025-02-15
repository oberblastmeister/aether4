open Std
open Cmdliner

type t =
  { verbose : bool
  ; path : string
  }
[@@deriving sexp_of]

open struct
  let ( let+ ) m f = Term.map f m
  let ( and+ ) = Term.product
  let flag info = Arg.value (Arg.flag info)
  let opt conv ~default info = Arg.value (Arg.opt conv default info)
end

let term : t Term.t =
  let+ verbose =
    let doc = "If present, print verbose debug information." in
    flag (Arg.info [ "v"; "verbose" ] ~doc)
  and+ path =
    let doc = "The source file $(docv) to compile." in
    Arg.(required (pos 0 (some non_dir_file) None (info [] ~doc ~docv:"FILE")))
  in
  { verbose; path }
;;

let with_args f =
  let cmd =
    Cmd.v
      (Cmd.info "c0c" ~doc:"Compile a c0c source file.")
      (let+ args = term in
       f args;
       ())
  in
  exit (Cmd.eval cmd)
;;
