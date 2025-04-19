open Std
open Aether4
open Ae_abs_x86_std
module Label = Ae_label_entity.Ident
module Entity = Ae_entity_std
module Ident = Entity.Ident
module Label_entity = Ae_label_entity
module Label_intern = Entity.Intern.String_to_name.Make_global (Label_entity.Witness) ()
module Temp_intern = Entity.Intern.String_to_name.Make_global (Temp_entity.Witness) ()

let lab = Label_intern.intern
let temp = Temp_intern.intern
let ins = Instr'.create_unindexed

let instrs =
  [ Instr.Mov { dst = Reg (temp "a3"); src = Imm 1l; size = Qword }
  ; Instr.Bin
      { dst = Reg (temp "w"); op = Sub; src1 = Reg (temp "a1"); src2 = Reg (temp "b1") }
  ; Instr.Bin
      { dst = Reg (temp "z"); op = Add; src1 = Reg (temp "a"); src2 = Reg (temp "b") }
  ; Instr.Mov { dst = Reg (temp "x"); src = Reg (temp "y"); size = Qword }
  ]
;;

let%expect_test "simple next use backwards transfer" =
  let next_uses_out = Ident.Map.of_alist_exn [ temp "x", 10; temp "z", 20 ] |> ref in
  begin
    let@: instr = List.rev instrs |> List.iter in
    print_s [%message (instr : Instr.t) (next_uses_out : int Temp.Map.t ref)];
    next_uses_out := Liveness.next_use_backwards_transfer instr !next_uses_out
  end;
  ();
  [%expect
    {|
    ((instr (Mov (dst (Reg x@1)) (src (Reg y@0)) (size Qword)))
     (next_uses_out ((x@1 10) (z@4 20))))
    ((instr (Bin (dst (Reg z@4)) (op Add) (src1 (Reg a@3)) (src2 (Reg b@2))))
     (next_uses_out ((y@0 0) (z@4 21))))
    ((instr (Bin (dst (Reg w@7)) (op Sub) (src1 (Reg a1@6)) (src2 (Reg b1@5))))
     (next_uses_out ((y@0 1) (b@2 0) (a@3 0))))
    ((instr (Mov (dst (Reg a3@8)) (src (Imm 1)) (size Qword)))
     (next_uses_out ((y@0 2) (b@2 1) (a@3 1) (b1@5 0) (a1@6 0))))
    |}]
;;

let%expect_test "compute deaths" =
  let live_out = Ident.Set.of_list_exn [ temp "x"; temp "z" ] in
  let block =
    List.map ~f:(fun i -> Instr'.create_unindexed i) instrs
    |> Arrayp.of_list
    |> Block.create (lab "start")
  in
  let deaths = Liveness.compute_deaths ~live_out block in
  print_s [%message (deaths : Temp.Set.t iarray)];
  ();
  [%expect {| (deaths (() (b1@5 a1@6) (b@2 a@3) (y@0))) |}]
;;
