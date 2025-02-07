open Std

module Option_array = struct
  module OA = Option_array

  let resize_array a size =
    let a' = OA.create ~len:size in
    OA.blit ~src:a ~dst:a' ~src_pos:0 ~dst_pos:0 ~len:(OA.length a);
    a'
  ;;

  let resize_for_index a index =
    if index >= Option_array.length a
    then (
      let new_size = max 4 (Int.round_up (index + 1) ~to_multiple_of:2) in
      resize_array a new_size)
    else a
  ;;
end
