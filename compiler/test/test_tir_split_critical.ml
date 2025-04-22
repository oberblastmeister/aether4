open Std
open Aether4
module Trace = Ae_trace
module Tir = Ae_tir_std
open Tir
module Entity = Ae_entity_std
module Ident = Entity.Ident
module Label = Ae_label
module Label_intern = Label.Intern.Make_global ()
module Temp_intern = Temp.Intern.Make_global ()

let lab = Label_intern.intern
let temp = Temp_intern.intern
let ins = Instr'.create_unindexed

let%expect_test "smoke" =
  let blocks =
    [ ( lab "start"
      , [ ins (Block_params [])
        ; ins (Nullary { dst = temp "const_bool"; op = Bool_const true })
        ; ins (Nullary { dst = temp "const_int"; op = Int_const 1234L })
        ; ins
            (Cond_jump
               { cond = temp "const_bool"
               ; b1 =
                   { label = lab "loop"; args = [ temp "const_int"; temp "const_bool" ] }
               ; b2 = { label = lab "done"; args = [ temp "const_int" ] }
               })
        ] )
    ; ( lab "done"
      , [ ins (Block_params [ { param = temp "ret"; ty = Int } ])
        ; ins (Ret { src = temp "ret"; ty = Int })
        ] )
    ; ( lab "loop"
      , [ ins
            (Block_params
               [ { param = temp "loop1"; ty = Int }; { param = temp "loop2"; ty = Bool } ])
        ; ins (Jump { label = lab "loop"; args = [ temp "loop1"; temp "loop2" ] })
        ] )
    ]
  in
  let blocks =
    blocks
    |> List.map ~f:(fun (lab, block) -> lab, Block.create lab (Arrayp.of_list block))
    |> Label.Map.of_alist_exn
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
  print_s [%message (func : Func.t)];
  let func = Split_critical.split func in
  print_s [%message (func : Func.t)];
  Check.check func |> Or_error.ok_exn;
  ();
  [%expect
    {|
    (func
     ((name main)
      (blocks
       ((loop@0
         ((label loop@0)
          (body
           (((i
              (Block_params
               (((param loop1@1) (ty Int)) ((param loop2@0) (ty Bool)))))
             (index 0) (info ()) (ann ()))
            ((i (Jump ((label loop@0) (args (loop1@1 loop2@0))))) (index 1)
             (info ()) (ann ()))))))
        (done@1
         ((label done@1)
          (body
           (((i (Block_params (((param ret@2) (ty Int))))) (index 0) (info ())
             (ann ()))
            ((i (Ret (src ret@2) (ty Int))) (index 1) (info ()) (ann ()))))))
        (start@2
         ((label start@2)
          (body
           (((i (Block_params ())) (index 0) (info ()) (ann ()))
            ((i (Nullary (dst const_bool@4) (op (Bool_const true)))) (index 1)
             (info ()) (ann ()))
            ((i (Nullary (dst const_int@3) (op (Int_const 1234)))) (index 2)
             (info ()) (ann ()))
            ((i
              (Cond_jump (cond const_bool@4)
               (b1 ((label loop@0) (args (const_int@3 const_bool@4))))
               (b2 ((label done@1) (args (const_int@3))))))
             (index 3) (info ()) (ann ()))))))))
      (start start@2) (next_temp_id 5) (next_label_id 3)))
    (func
     ((name main)
      (blocks
       ((loop@0
         ((label loop@0)
          (body
           (((i
              (Block_params
               (((param loop1@1) (ty Int)) ((param loop2@0) (ty Bool)))))
             (index 0) (info ()) (ann ()))
            ((i (Jump ((label loop@0) (args (loop1@1 loop2@0))))) (index 1)
             (info ()) (ann ()))))))
        (done@1
         ((label done@1)
          (body
           (((i (Block_params (((param ret@2) (ty Int))))) (index 0) (info ())
             (ann ()))
            ((i (Ret (src ret@2) (ty Int))) (index 1) (info ()) (ann ()))))))
        (start@2
         ((label start@2)
          (body
           (((i (Block_params ())) (index 0) (info ()) (ann ()))
            ((i (Nullary (dst const_bool@4) (op (Bool_const true)))) (index 1)
             (info ()) (ann ()))
            ((i (Nullary (dst const_int@3) (op (Int_const 1234)))) (index 2)
             (info ()) (ann ()))
            ((i
              (Cond_jump (cond const_bool@4) (b1 ((label loop@3) (args ())))
               (b2 ((label done@1) (args (const_int@3))))))
             (index 3) (info ()) (ann ()))))))
        (loop@3
         ((label loop@3)
          (body
           (((i (Block_params ())) (index 0) (info ()) (ann ()))
            ((i (Jump ((label loop@0) (args (const_int@3 const_bool@4)))))
             (index 1) (info ()) (ann ()))))))))
      (start start@2) (next_temp_id 5) (next_label_id 4)))
    |}]
;;
