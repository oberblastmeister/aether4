let fail () = failwith "" in
let open Result.Let_syntax in
let res =
  let%bind_fail (Some _) = Ok None in
  Ok ()
in
()
