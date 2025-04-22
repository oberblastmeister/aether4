open Test_abs_x86_import
open Make_intern ()

(* let check coloring instr =
  let  in
  let open Table.Syntax in
  let frame_builder = Frame.Builder.create () in
  let slot () = Frame.Builder.alloc ~name:"spilled_color" frame_builder Qword in
  let max_color = 6 in
  let spilled = Int_table.of_list [ 0, slot (); 1, slot (); 2, slot () ] in
  let coloring = Table.of_list coloring in
  let get_evicted_temp_and_slot_for_color =
    let color_to_global_evicted = Int_table.create () in
    fun color ->
      Int_table.find_or_add color_to_global_evicted color ~default:(fun () ->
        let temp = Temp_intern.fresh ~name:"evicted" () in
        let stack_slot = Frame.Builder.alloc ~name:"evicted_slot" frame_builder Qword in
        coloring.!(temp) <- color;
        temp, stack_slot)
  in
  let instrs_before, instr, instrs_after =
    Regalloc.spill_instr
      spilled
      get_evicted_temp_and_slot_for_color
      coloring
      max_color
      instr
  in
  let instrs = instrs_before @ [ instr ] @ instrs_after in
  print_s [%sexp (instrs : Instr.t list)];
  ()
;;

let%expect_test _ =
  check
    [ temp "tmp1", 0; temp "tmp2", 1; temp "tmp3", 2 ]
    (Bin
       { dst = Reg (temp "tmp1")
       ; op = Add
       ; src1 = Reg (temp "tmp2")
       ; src2 = Reg (temp "tmp3")
       });
  [%expect
    {|
    ((Mov (dst (Stack_slot evicted_slot@3)) (src (Reg evicted@3)) (size Qword))
     (Mov (dst (Reg evicted@3)) (src (Stack_slot spilled_color@1)) (size Qword))
     (Mov (dst (Stack_slot evicted_slot@4)) (src (Reg evicted@4)) (size Qword))
     (Mov (dst (Reg evicted@4)) (src (Stack_slot spilled_color@0)) (size Qword))
     (Mov (dst (Stack_slot evicted_slot@5)) (src (Reg evicted@5)) (size Qword))
     (Bin (dst (Reg evicted@5)) (op Add) (src1 (Reg evicted@3))
      (src2 (Reg evicted@4)))
     (Mov (dst (Stack_slot spilled_color@2)) (src (Reg evicted@5)) (size Qword))
     (Mov (dst (Reg evicted@5)) (src (Stack_slot evicted_slot@5)) (size Qword))
     (Mov (dst (Reg evicted@4)) (src (Stack_slot evicted_slot@4)) (size Qword))
     (Mov (dst (Reg evicted@3)) (src (Stack_slot evicted_slot@3)) (size Qword)))
    |}]
;;

let%expect_test _ =
  check
    [ temp "tmp1", 3; temp "tmp2", 4; temp "tmp3", 5 ]
    (Bin
       { dst = Reg (temp "tmp1")
       ; op = Add
       ; src1 = Reg (temp "tmp2")
       ; src2 = Reg (temp "tmp3")
       });
  [%expect
    {| ((Bin (dst (Reg tmp1@2)) (op Add) (src1 (Reg tmp2@1)) (src2 (Reg tmp3@0)))) |}]
;;

let%expect_test _ =
  check
    [ temp "tmp1", 1; temp "tmp2", 4; temp "tmp3", 5 ]
    (Bin
       { dst = Reg (temp "tmp1")
       ; op = Add
       ; src1 = Reg (temp "tmp2")
       ; src2 = Reg (temp "tmp3")
       });
  [%expect
    {|
    ((Mov (dst (Stack_slot evicted_slot@3)) (src (Reg evicted@6)) (size Qword))
     (Bin (dst (Reg evicted@6)) (op Add) (src1 (Reg tmp2@1)) (src2 (Reg tmp3@0)))
     (Mov (dst (Stack_slot spilled_color@1)) (src (Reg evicted@6)) (size Qword))
     (Mov (dst (Reg evicted@6)) (src (Stack_slot evicted_slot@3)) (size Qword)))
    |}]
;; *)
