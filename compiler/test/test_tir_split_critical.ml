open Std
open Aether4
module Trace = Ae_trace
module Tir = Ae_tir_std
open Tir
module Entity = Ae_entity_std
module Ident = Entity.Ident
module Label_entity = Ae_label_entity
module Label = Label_entity.Ident
module Label_intern = Entity.Intern.String_to_name.Make_global (Label_entity.Witness) ()
module Temp_intern = Entity.Intern.String_to_name.Make_global (Temp_entity.Witness) ()

let lab = Label_intern.intern
let temp = Temp_intern.intern
let ins = Instr'.create_unindexed

let%expect_test "smoke" =
  let blocks =
    [ ( lab "start"
      , [ ins (Block_params { temps = [] })
        ; ins (Nullary { dst = temp "const_bool"; op = BoolConst true })
        ; ins (Nullary { dst = temp "const_int"; op = IntConst 1234L })
        ; ins
            (Cond_jump
               { cond = temp "const_bool"
               ; b1 =
                   Block_call.create
                     (lab "loop")
                     ~args:[ temp "const_int"; temp "const_bool" ]
               ; b2 = Block_call.create (lab "done") ~args:[ temp "const_int" ]
               })
        ] )
    ; ( lab "done"
      , [ ins (Block_params { temps = [ temp "ret", Int ] })
        ; ins (Ret { src = temp "ret"; ty = Int })
        ] )
    ; ( lab "loop"
      , [ ins (Block_params { temps = [ temp "loop1", Int; temp "loop2", Bool ] })
        ; ins (Jump (Block_call.create (lab "loop")))
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
    }
  in
  Check.check func |> Or_error.ok_exn;
  let func = Split_critical.split func in
  print_s [%message (func : Func.t)];
  Check.check func |> Or_error.ok_exn;
  ();
  [%expect
    {|
    (func
     ((name main)
      (blocks
       ((0
         ((key loop@0)
          (data
           ((label loop@0)
            (body
             (((i (Block_params (temps ((loop1@1 Int) (loop2@0 Bool)))))
               (index 0) (info ()))
              ((i (Jump ((label loop@0) (args ())))) (index 1) (info ()))))))))
        (1
         ((key done@1)
          (data
           ((label done@1)
            (body
             (((i (Block_params (temps ((ret@2 Int))))) (index 0) (info ()))
              ((i (Ret (src ret@2) (ty Int))) (index 1) (info ()))))))))
        (2
         ((key start@2)
          (data
           ((label start@2)
            (body
             (((i (Block_params (temps ()))) (index 0) (info ()))
              ((i (Nullary (dst const_bool@4) (op (BoolConst true)))) (index 1)
               (info ()))
              ((i (Nullary (dst const_int@3) (op (IntConst 1234)))) (index 2)
               (info ()))
              ((i
                (Cond_jump (cond const_bool@4) (b1 ((label loop@3) (args ())))
                 (b2 ((label done@1) (args (const_int@3))))))
               (index 3) (info ()))))))))
        (3
         ((key loop@3)
          (data
           ((label loop@3)
            (body
             (((i (Block_params (temps ()))) (index 0) (info ()))
              ((i (Jump ((label loop@0) (args (const_int@3 const_bool@4)))))
               (index 1) (info ()))))))))))
      (start start@2) (next_temp_id 5) (next_label_id 4)))
    |}]
;;
