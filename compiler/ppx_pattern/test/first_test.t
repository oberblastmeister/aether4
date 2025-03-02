Creates an file with some OCaml code using bash heredoc
  $ cat > input.ml <<EOF
  > let fail () = failwith "" in
  > let open Result.Let_syntax in
  > let res =
  >   let%bind_fail (Some _) = Ok None in
  >   Ok ()
  > in
  > ()
  > EOF
 
Run the executable from input.ml and print the output to stdout
  $ ./standalone.exe --impl input.ml
  ;;let fail () = failwith "" in
    let open Result.Let_syntax in
      let res = [%bind match Ok None with | Some _ -> Ok () | _ -> fail ()] in
      ()
