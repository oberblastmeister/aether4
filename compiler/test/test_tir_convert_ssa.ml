open Std
open Aether4
open Ae_trace
module Driver = Ae_driver
module C0 = Ae_c0_std
module Entity_graph_utils = Ae_entity_graph_utils
module Tir = Ae_tir_std
open Tir
module Dominators = Ae_dominators
module Label = Ae_label_entity.Ident
module Entity = Ae_entity_std
module Ident = Entity.Ident
module Label_entity = Ae_label_entity
module Label_intern = Entity.Intern.String_to_name.Make_global (Label_entity.Witness) ()
module Temp_intern = Entity.Intern.String_to_name.Make_global (Temp_entity.Witness) ()

let lab = Label_intern.intern
let temp = Temp_intern.intern
let ins = Instr'.create_unindexed

let check s =
  let func = Driver.compile_source_to_tir s |> Or_error.ok_exn in
  (* let placements = Tir.Convert_ssa.compute_phi_placements func in *)
  (* print_s [%message (placements : Tir.Temp.t list Label.Table.t)]; *)
  (* let func = Tir.Convert_ssa.convert func in *)
  Tir.Check_ssa.check func |> Or_error.ok_exn;
  let live_in, live_out =
    Tir.Liveness.compute ~pred_table:(Tir.Func.pred_table func) func
  in
  print_s
    [%message (live_in : Tir.Liveness.Live_set.t) (live_out : Tir.Liveness.Live_set.t)];
  print_s [%message (func : Tir.Func.t)];
  ()
;;

let%expect_test "" =
  let blocks =
    [ ( lab "start"
      , [ ins (Block_params [])
        ; ins (Nullary { dst = temp "b1"; op = Bool_const true })
        ; ins
            (Cond_jump
               { cond = temp "b1"
               ; b1 = { label = lab "then"; args = [] }
               ; b2 = { label = lab "else"; args = [] }
               })
        ] )
    ; ( lab "then"
      , [ ins (Block_params [])
        ; ins (Nullary { dst = temp "i1"; op = Int_const 1L })
        ; ins (Nullary { dst = temp "i4"; op = Int_const 4L })
        ; ins (Jump { label = lab "join"; args = [ temp "i1" ] })
        ] )
    ; ( lab "else"
      , [ ins (Block_params [])
        ; ins (Nullary { dst = temp "i2"; op = Int_const 2L })
        ; ins (Nullary { dst = temp "i4"; op = Int_const 5L })
        ; ins (Jump { label = lab "join"; args = [ temp "i2" ] })
        ] )
    ; ( lab "join"
      , [ ins (Block_params [ { param = temp "i3"; ty = Int } ])
        ; ins (Bin { dst = temp "ret"; src1 = temp "i3"; src2 = temp "i4"; op = Add })
        ; ins (Ret { src = temp "ret"; ty = Int })
        ] )
    ]
  in
  let blocks =
    blocks
    |> List.map ~f:(fun (lab, block) -> lab, Block.create lab (Arrayp.of_list block))
    |> Ident.Map.of_alist_exn
  in
  let func =
    { Func.name = "main"
    ; Func.blocks
    ; start = lab "start"
    ; next_temp_id = Temp_intern.next_id ()
    ; next_label_id = Label_intern.next_id ()
    ; data = ()
    }
  in
  let func = Tir.Convert_ssa.convert ~renumber:() func in
  print_s [%message (func : Func.t)];
  ();
  [%expect
    {|
    (func
     ((name main)
      (blocks
       ((join@0
         ((label join@0)
          (body
           (((i (Block_params (((param i3@5) (ty Int)) ((param i4@6) (ty Int)))))
             (index 0) (info ()) (ann ()))
            ((i (Bin (dst ret@7) (op Add) (src1 i3@5) (src2 i4@6))) (index 1)
             (info ()) (ann ()))
            ((i (Ret (src ret@7) (ty Int))) (index 2) (info ()) (ann ()))))))
        (else@1
         ((label else@1)
          (body
           (((i (Block_params ())) (index 0) (info ()) (ann ()))
            ((i (Nullary (dst i2@3) (op (Int_const 2)))) (index 1) (info ())
             (ann ()))
            ((i (Nullary (dst i4@4) (op (Int_const 5)))) (index 2) (info ())
             (ann ()))
            ((i (Jump ((label join@0) (args (i2@3 i4@4))))) (index 3) (info ())
             (ann ()))))))
        (then@2
         ((label then@2)
          (body
           (((i (Block_params ())) (index 0) (info ()) (ann ()))
            ((i (Nullary (dst i1@1) (op (Int_const 1)))) (index 1) (info ())
             (ann ()))
            ((i (Nullary (dst i4@2) (op (Int_const 4)))) (index 2) (info ())
             (ann ()))
            ((i (Jump ((label join@0) (args (i1@1 i4@2))))) (index 3) (info ())
             (ann ()))))))
        (start@3
         ((label start@3)
          (body
           (((i (Block_params ())) (index 0) (info ()) (ann ()))
            ((i (Nullary (dst b1@0) (op (Bool_const true)))) (index 1) (info ())
             (ann ()))
            ((i
              (Cond_jump (cond b1@0) (b1 ((label then@2) (args ())))
               (b2 ((label else@1) (args ())))))
             (index 2) (info ()) (ann ()))))))))
      (start start@3) (next_temp_id 8) (next_label_id 4) (data ())))
    |}]
;;
(* let%expect_test "nothing" =
  check
    {|
      int main() {
          int first = 3;
          return first;
      }
      |};
  [%expect
    {|
    ((live_in ()) (live_out ()))
    (func
     ((name main)
      (blocks
       ((start@0
         ((label start@0)
          (body
           (((i (Block_params (temps ()))) (index 0) (info ()))
            ((i (Nullary (dst tmp@3) (op (Int_const 3)))) (index 1) (info ()))
            ((i (Unary (dst first@4) (op (Copy Int)) (src tmp@3))) (index 2)
             (info ()))
            ((i (Unary (dst ret@5) (op (Copy Int)) (src first@4))) (index 3)
             (info ()))
            ((i (Ret (src ret@5) (ty Int))) (index 4) (info ()))))))))
      (start start@0) (next_temp_id 6) (next_label_id 1)))
    |}]
;;

let%expect_test "smoke" =
  check
    {|
      int main() {
          int first = 3;
          int second = 1;
          bool another = true;
          if (another) {
              second += 1;
          } else {
              first += second;
          }
          return first + second;
      }
      |};
  [%expect
    {|
    ((live_in ((then@1 (first@15 second@17)) (else@2 (first@15 second@17))))
     (live_out ((start@3 (first@15 second@17)))))
    (func
     ((name main)
      (blocks
       ((join@0
         ((label join@0)
          (body
           (((i (Block_params (temps ((second@27 Int) (first@28 Int)))))
             (index 0) (info ()))
            ((i (Unary (dst lhs@29) (op (Copy Int)) (src first@28))) (index 1)
             (info ()))
            ((i (Unary (dst rhs@30) (op (Copy Int)) (src second@27))) (index 2)
             (info ()))
            ((i (Bin (dst ret@31) (op Add) (src1 lhs@29) (src2 rhs@30)))
             (index 3) (info ()))
            ((i (Ret (src ret@31) (ty Int))) (index 4) (info ()))))))
        (then@1
         ((label then@1)
          (body
           (((i (Block_params (temps ()))) (index 0) (info ()))
            ((i (Unary (dst lhs@24) (op (Copy Int)) (src second@17))) (index 1)
             (info ()))
            ((i (Nullary (dst rhs@25) (op (Int_const 1)))) (index 2) (info ()))
            ((i (Bin (dst second@26) (op Add) (src1 lhs@24) (src2 rhs@25)))
             (index 3) (info ()))
            ((i (Jump ((label join@0) (args (second@26 first@15))))) (index 4)
             (info ()))))))
        (else@2
         ((label else@2)
          (body
           (((i (Block_params (temps ()))) (index 0) (info ()))
            ((i (Unary (dst lhs@21) (op (Copy Int)) (src first@15))) (index 1)
             (info ()))
            ((i (Unary (dst rhs@22) (op (Copy Int)) (src second@17))) (index 2)
             (info ()))
            ((i (Bin (dst first@23) (op Add) (src1 lhs@21) (src2 rhs@22)))
             (index 3) (info ()))
            ((i (Jump ((label join@0) (args (second@17 first@23))))) (index 4)
             (info ()))))))
        (start@3
         ((label start@3)
          (body
           (((i (Block_params (temps ()))) (index 0) (info ()))
            ((i (Nullary (dst tmp@14) (op (Int_const 3)))) (index 1) (info ()))
            ((i (Unary (dst first@15) (op (Copy Int)) (src tmp@14))) (index 2)
             (info ()))
            ((i (Nullary (dst tmp@16) (op (Int_const 1)))) (index 3) (info ()))
            ((i (Unary (dst second@17) (op (Copy Int)) (src tmp@16))) (index 4)
             (info ()))
            ((i (Nullary (dst tmp@18) (op (Bool_const true)))) (index 5)
             (info ()))
            ((i (Unary (dst another@19) (op (Copy Bool)) (src tmp@18))) (index 6)
             (info ()))
            ((i (Unary (dst cond@20) (op (Copy Bool)) (src another@19)))
             (index 7) (info ()))
            ((i
              (Cond_jump (cond cond@20) (b1 ((label then@1) (args ())))
               (b2 ((label else@2) (args ())))))
             (index 8) (info ()))))))))
      (start start@3) (next_temp_id 32) (next_label_id 4)))
    |}]
;; *)
