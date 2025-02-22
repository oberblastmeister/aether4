open Std

let apply_inserts dummy inserts array =
  let new_array = Array.create ~len:(List.length inserts + Array.length array) dummy in
  let new_array_ix = ref (Array.length new_array - 1) in
  let inserts =
    let arr = List.to_array inserts in
    let compare = fun (i, _) (j, _) -> compare i j in
    Array.stable_sort arr ~compare;
    arr
  in
  let inserts_ix = ref (Array.length inserts - 1) in
  let do_inserts i =
    while !inserts_ix >= 0 && fst inserts.(!inserts_ix) > i do
      let _, x = inserts.(!inserts_ix) in
      decr inserts_ix;
      Array.set new_array !new_array_ix x;
      decr new_array_ix
    done
  in
  Array.iteri array ~f:(fun i _ ->
    let i = Array.length array - 1 - i in
    let x = array.(i) in
    do_inserts i;
    Array.set new_array !new_array_ix x;
    decr new_array_ix;
    ());
  do_inserts (-1);
  new_array
;;
