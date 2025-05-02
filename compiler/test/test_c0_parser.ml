open Std
open Aether4
module C0 = Ae_c0_std
module Spanned = Ae_spanned
module Trace = Ae_trace

let check s =
  let tokens = C0.Lexer.tokenize s in
  let program = C0.Parser.parse (Array.of_list tokens) in
  print_s [%sexp (program : C0.Cst.program Or_error.t)];
  ()
;;

let%expect_test "simple" =
  check
    {|
    int bruh() {
    
    }
  |};
  [%expect
    {|
    (Ok
     ((Func
       ((is_extern false) (ty (Int 2:5-8)) (name ((t bruh) (span 2:9-13)))
        (params ()) (body (((block ()) (span [2,16]-[4,6])))) (span [2,5]-[4,6])))))
    |}]
;;

let%expect_test "pointer" =
  let s =
    {|
    void main() {
      int * first;
      int bruh = 1234;
      bruh * first;
    }
    
    typedef bruh int;
    
    void another() {
      bruh * first;
      int bruh;
      bruh * first;
    }
  |}
  in
  check s;
  [%expect
    {|
    (Ok
     ((Func
       ((is_extern false) (ty (Void 2:5-9)) (name ((t main) (span 2:10-14)))
        (params ())
        (body
         (((block
            ((Decl (ty (Pointer (ty (Int 3:7-10)) (span 3:7-12)))
              (names (((t first) (span 3:13-18)))) (expr ()) (span 3:7-12))
             (Decl (ty (Int 4:7-10)) (names (((t bruh) (span 4:11-15))))
              (expr ((Int_const ((t 1234) (span 4:18-22))))) (span 4:7-22))
             (Effect
              (Bin (lhs (Var ((t bruh) (span 5:7-11)))) (op Mul)
               (rhs (Var ((t first) (span 5:14-19)))) (span 5:7-19)))))
           (span [2,17]-[6,6]))))
        (span [2,5]-[6,6])))
      (Typedef (ty (Int 8:18-21)) (name ((t bruh) (span 8:13-17))) (span 8:5-17))
      (Func
       ((is_extern false) (ty (Void 10:5-9)) (name ((t another) (span 10:10-17)))
        (params ())
        (body
         (((block
            ((Decl
              (ty
               (Pointer (ty (Ty_var ((t bruh) (span 11:7-11)))) (span 11:7-13)))
              (names (((t first) (span 11:14-19)))) (expr ()) (span 11:7-13))
             (Decl (ty (Int 12:7-10)) (names (((t bruh) (span 12:11-15))))
              (expr ()) (span 12:7-10))
             (Effect
              (Bin (lhs (Var ((t bruh) (span 13:7-11)))) (op Mul)
               (rhs (Var ((t first) (span 13:14-19)))) (span 13:7-19)))))
           (span [10,20]-[14,6]))))
        (span [10,5]-[14,6])))))
    |}]
;;

let%expect_test "simple decl" =
  check
    {|
    int first() {
      int first = 12 + 1234 % 1234 * 12 / 2;
      int second = 1234 + 12 + 12;
    }
  |};
  [%expect
    {|
    (Ok
     ((Func
       ((is_extern false) (ty (Int 2:5-8)) (name ((t first) (span 2:9-14)))
        (params ())
        (body
         (((block
            ((Decl (ty (Int 3:7-10)) (names (((t first) (span 3:11-16))))
              (expr
               ((Bin (lhs (Int_const ((t 12) (span 3:19-21)))) (op Add)
                 (rhs
                  (Bin
                   (lhs
                    (Bin
                     (lhs
                      (Bin (lhs (Int_const ((t 1234) (span 3:24-28)))) (op Mod)
                       (rhs (Int_const ((t 1234) (span 3:31-35))))
                       (span 3:24-35)))
                     (op Mul) (rhs (Int_const ((t 12) (span 3:38-40))))
                     (span 3:24-40)))
                   (op Div) (rhs (Int_const ((t 2) (span 3:43)))) (span 3:24-44)))
                 (span 3:19-44))))
              (span 3:7-44))
             (Decl (ty (Int 4:7-10)) (names (((t second) (span 4:11-17))))
              (expr
               ((Bin
                 (lhs
                  (Bin (lhs (Int_const ((t 1234) (span 4:20-24)))) (op Add)
                   (rhs (Int_const ((t 12) (span 4:27-29)))) (span 4:20-29)))
                 (op Add) (rhs (Int_const ((t 12) (span 4:32-34))))
                 (span 4:20-34))))
              (span 4:7-34))))
           (span [2,17]-[5,6]))))
        (span [2,5]-[5,6])))))
    |}]
;;

let%expect_test "simple control flow" =
  check
    {|
    int main() {
      int i = 1234;
      if (b) {
        another = 1243;
      }
    }
  |};
  [%expect
    {|
    (Ok
     ((Func
       ((is_extern false) (ty (Int 2:5-8)) (name ((t main) (span 2:9-13)))
        (params ())
        (body
         (((block
            ((Decl (ty (Int 3:7-10)) (names (((t i) (span 3:11))))
              (expr ((Int_const ((t 1234) (span 3:15-19))))) (span 3:7-19))
             (If (cond (Var ((t b) (span 4:11))))
              (body1
               (Block
                ((block
                  ((Assign
                    ((lvalue (Lvalue_var ((t another) (span 5:9-16))))
                     (op Id_assign) (expr (Int_const ((t 1243) (span 5:19-23))))
                     (span 5:9-23)))))
                 (span [4,14]-[6,8]))))
              (body2 ()) (span [4,7]-[6,8]))))
           (span [2,16]-[7,6]))))
        (span [2,5]-[7,6])))))
    |}]
;;

let%expect_test "simple assign" =
  check
    {|
      int first() {
        first *= 12 + 12;
        ((another)) %= 12 / 12;      
      }
    |};
  [%expect
    {|
    (Ok
     ((Func
       ((is_extern false) (ty (Int 2:7-10)) (name ((t first) (span 2:11-16)))
        (params ())
        (body
         (((block
            ((Assign
              ((lvalue (Lvalue_var ((t first) (span 3:9-14)))) (op Mul_assign)
               (expr
                (Bin (lhs (Int_const ((t 12) (span 3:18-20)))) (op Add)
                 (rhs (Int_const ((t 12) (span 3:23-25)))) (span 3:18-25)))
               (span 3:9-25)))
             (Assign
              ((lvalue (Lvalue_var ((t another) (span 4:11-18)))) (op Mod_assign)
               (expr
                (Bin (lhs (Int_const ((t 12) (span 4:24-26)))) (op Div)
                 (rhs (Int_const ((t 12) (span 4:29-31)))) (span 4:24-31)))
               (span 4:11-31)))))
           (span [2,19]-[5,8]))))
        (span [2,7]-[5,8])))))
    |}]
;;

let%expect_test "bool" =
  check
    {|
    int main() {
          int first = 0;
          int second = 1234;
          bool third = true;
          return first + second;
        }
  |};
  [%expect
    {|
    (Ok
     ((Func
       ((is_extern false) (ty (Int 2:5-8)) (name ((t main) (span 2:9-13)))
        (params ())
        (body
         (((block
            ((Decl (ty (Int 3:11-14)) (names (((t first) (span 3:15-20))))
              (expr ((Int_const ((t 0) (span 3:23))))) (span 3:11-24))
             (Decl (ty (Int 4:11-14)) (names (((t second) (span 4:15-21))))
              (expr ((Int_const ((t 1234) (span 4:24-28))))) (span 4:11-28))
             (Decl (ty (Bool 5:11-15)) (names (((t third) (span 5:16-21))))
              (expr ((Bool_const ((t true) (span 5:24-28))))) (span 5:11-28))
             (Return
              (expr
               ((Bin (lhs (Var ((t first) (span 6:18-23)))) (op Add)
                 (rhs (Var ((t second) (span 6:26-32)))) (span 6:18-32))))
              (span 6:11-32))))
           (span [2,16]-[7,10]))))
        (span [2,5]-[7,10])))))
    |}]
;;

let%expect_test "typedef" =
  check
    {|
  typedef testing int;
  
  typedef another int;
  
  testing main() {
    testing first = 0;
    first(second(first, third));
  }
  
  testing another() {
    testing second = 1234;
  }
  
  testing another();
  
  extern testing another();
  
  extern void print_char(testing c);
  |};
  [%expect
    {|
    (Ok
     ((Typedef (ty (Int 2:19-22)) (name ((t testing) (span 2:11-18)))
       (span 2:3-18))
      (Typedef (ty (Int 4:19-22)) (name ((t another) (span 4:11-18)))
       (span 4:3-18))
      (Func
       ((is_extern false) (ty (Ty_var ((t testing) (span 6:3-10))))
        (name ((t main) (span 6:11-15))) (params ())
        (body
         (((block
            ((Decl (ty (Ty_var ((t testing) (span 7:5-12))))
              (names (((t first) (span 7:13-18))))
              (expr ((Int_const ((t 0) (span 7:21))))) (span 7:5-22))
             (Effect
              (Call (func ((t first) (span 8:5-10)))
               (args
                ((Call (func ((t second) (span 8:11-17)))
                  (args
                   ((Var ((t first) (span 8:18-23)))
                    (Var ((t third) (span 8:25-30)))))
                  (span 8:11-31))))
               (span 8:5-32)))))
           (span [6,18]-[9,4]))))
        (span [6,3]-[9,4])))
      (Func
       ((is_extern false) (ty (Ty_var ((t testing) (span 11:3-10))))
        (name ((t another) (span 11:11-18))) (params ())
        (body
         (((block
            ((Decl (ty (Ty_var ((t testing) (span 12:5-12))))
              (names (((t second) (span 12:13-19))))
              (expr ((Int_const ((t 1234) (span 12:22-26))))) (span 12:5-26))))
           (span [11,21]-[13,4]))))
        (span [11,3]-[13,4])))
      (Func
       ((is_extern false) (ty (Ty_var ((t testing) (span 15:3-10))))
        (name ((t another) (span 15:11-18))) (params ()) (body ())
        (span 15:3-20)))
      (Func
       ((is_extern true) (ty (Ty_var ((t testing) (span 17:10-17))))
        (name ((t another) (span 17:18-25))) (params ()) (body ())
        (span 17:10-27)))
      (Func
       ((is_extern true) (ty (Void 19:10-14))
        (name ((t print_char) (span 19:15-25)))
        (params
         (((var ((t c) (span 19:34))) (ty (Ty_var ((t testing) (span 19:26-33))))
           (span 19:26-35))))
        (body ()) (span 19:10-36)))))
    |}]
;;

let%expect_test _ =
  check
    {|
    int main() {
      int first = 0;
      int* p = alloc(int);
      int bruh = *first;
      assert true;
    }
    |};
  [%expect
    {|
    (Ok
     ((Func
       ((is_extern false) (ty (Int 2:5-8)) (name ((t main) (span 2:9-13)))
        (params ())
        (body
         (((block
            ((Decl (ty (Int 3:7-10)) (names (((t first) (span 3:11-16))))
              (expr ((Int_const ((t 0) (span 3:19))))) (span 3:7-20))
             (Decl (ty (Pointer (ty (Int 4:7-10)) (span 4:7-11)))
              (names (((t p) (span 4:12))))
              (expr ((Alloc (ty (Int 4:22-25)) (span 4:16-26)))) (span 4:7-26))
             (Decl (ty (Int 5:7-10)) (names (((t bruh) (span 5:11-15))))
              (expr
               ((Unary (op Deref) (expr (Var ((t first) (span 5:19-24))))
                 (span 5:18-24))))
              (span 5:7-24))
             (Assert (expr (Bool_const ((t true) (span 6:14-18)))) (span 6:7-18))))
           (span [2,16]-[7,6]))))
        (span [2,5]-[7,6])))))
    |}]
;;
