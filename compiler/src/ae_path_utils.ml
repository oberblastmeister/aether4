open Std

let get_workspace_root_in_build_dir path =
  let parts = Filename.parts path in
  let path_before_build, path_after_build =
    List.split_while parts ~f:(fun part -> not (Filename.equal part "_build"))
  in
  match path_after_build with
  | [] -> Filename.dirname path
  | _ ->
    let path_before_build =
      List.fold_left
        (List.tl_exn path_before_build)
        ~init:(List.hd_exn path_before_build)
        ~f:Filename.concat
    in
    path_before_build
;;
