open! Core
open! Import
module Env = Llama_typing.Env

let test_pattern pattern typ ~env =
  let constraints, _ =
    Constraints.For_testing.check_pattern pattern typ ~env |> Type_error.ok_exn
  in
  print_s [%message (constraints : Constraints.t)]
;;

let test_match scrutinee cases ~env =
  let _, constraints =
    Constraints.infer { desc = Match { scrutinee; cases }; loc = Span.dummy } ~env
    |> Type_error.ok_exn
  in
  print_s [%message (constraints : Constraints.t)]
;;

let%expect_test "option pattern" =
  Type.Var.For_testing.reset_counter ();
  Type.Id.For_testing.reset_counter ();
  let env =
    Env.with_type_declaration
      (Env.with_constructors
         Env.empty
         [ { value = Constructor.of_string "Some"; loc = Span.dummy }, () ]
         ~type_name:(Type_name.of_string "option")
       |> Type_error.ok_exn)
      (Type_name.of_string "option")
      (let arg = Type.Var.create () in
       { shape =
           Variant
             { constructors =
                 [ { value = Constructor.of_string "None"; loc = Span.dummy }, None
                 ; ( { value = Constructor.of_string "Some"; loc = Span.dummy }
                   , Some (Var arg) )
                 ]
             ; id = Type.Id.create ()
             }
       ; args = [ arg ]
       ; loc = Span.dummy
       })
  in
  let v = Type.Var.create () in
  print_s [%message "variable we care about:" (v : Type.Var.t)];
  test_pattern
    { desc =
        Construct
          ( Located.dummy (Constructor.of_string "Some")
          , Some { desc = Var (Located.dummy (Ident.of_string "x")); loc = Span.dummy } )
    ; loc = Span.dummy
    }
    (Type.Var v)
    ~env;
  [%expect
    {|
    ("variable we care about:" (v 1))
    (constraints ((
      Same_type
      (Var 1)
      (Apply ((value option) (loc (:0:-1 :0:-1))) ((Var 2)))
      (::
        (Pattern_should_have_type
          ((loc (:0:-1 :0:-1))
           (desc (
             Construct (
               ((value Some) (loc (:0:-1 :0:-1)))
               (((loc (:0:-1 :0:-1)) (desc (Var ((value x) (loc (:0:-1 :0:-1)))))))))))
          (Var 1))
        ()))))
    |}]
;;

let%expect_test "option match" =
  Type.Var.For_testing.reset_counter ();
  Type.Id.For_testing.reset_counter ();
  let x = Ident.of_string "x" in
  let env =
    Env.with_var
      (Env.with_type_declaration
         (Env.with_constructors
            Env.empty
            [ { value = Constructor.of_string "Some"; loc = Span.dummy }, () ]
            ~type_name:(Type_name.of_string "option")
          |> Type_error.ok_exn)
         (Type_name.of_string "option")
         (let arg = Type.Var.create () in
          { shape =
              Variant
                { constructors =
                    [ { value = Constructor.of_string "None"; loc = Span.dummy }, None
                    ; ( { value = Constructor.of_string "Some"; loc = Span.dummy }
                      , Some (Var arg) )
                    ]
                ; id = Type.Id.create ()
                }
          ; args = [ arg ]
          ; loc = Span.dummy
          }))
      x
      (Type.Poly.mono (Var (Type.Var.create ())))
  in
  print_s [%message (env : Env.t)];
  test_match
    { desc = Var (Ident.of_string "x"); loc = Span.dummy }
    [ ( { desc =
            Construct
              ( Located.dummy (Constructor.of_string "Some")
              , Some
                  { desc = Var (Located.dummy (Ident.of_string "y")); loc = Span.dummy }
              )
        ; loc = Span.dummy
        }
      , { desc = Var (Ident.of_string "y"); loc = Span.dummy } )
    ]
    ~env;
  [%expect
    {|
    (env (
      (values ((x ((quantifiers ()) (ty (Var 0))))))
      (type_declarations ((
        option (
          (shape (
            Variant
            (constructors (
              (((value None) (loc (:0:-1 :0:-1))) ())
              (((value Some) (loc (:0:-1 :0:-1))) ((Var 1)))))
            (id 0)))
          (args (1))
          (loc (:0:-1 :0:-1))))))
      (constructors ((Some option)))
      (fields ())))
    (constraints ((
      Same_type
      (Var 0)
      (Apply ((value option) (loc (:0:-1 :0:-1))) ((Var 2)))
      (::
        (Pattern_should_have_type
          ((loc (:0:-1 :0:-1))
           (desc (
             Construct (
               ((value Some) (loc (:0:-1 :0:-1)))
               (((loc (:0:-1 :0:-1)) (desc (Var ((value y) (loc (:0:-1 :0:-1)))))))))))
          (Var 0))
        ()))))
    |}]
;;

(* let test_fragment ?(pp_ast = false) code =
  Type.Var.For_testing.reset_counter ();
  Type.Id.For_testing.reset_counter ();
  let ast =
    let lexbuf = Lexing.from_string code in
    let ast = Llama_frontend.Parser.program Llama_frontend.Lexer.read lexbuf in
    if pp_ast then Pretty_print.pp_ast Format.std_formatter ast;
    ast
  in
  let result = Constraints.type_ast ast in
  print_s [%message (result : (Env.t * Constraints.t) Or_error.t)]
;;

let%expect_test "int" =
  test_fragment "let x = 1";
  [%expect
    {|
    (result (
      Ok (
        ((values ((x ((quantifiers ()) (ty (Intrinsic Int))))))
         (type_declarations ())
         (constructors      ())
         (fields            ()))
        ())))
    |}]
;;

let%expect_test "option" =
  test_fragment
    {|
    type 'a option = | None | Some of 'a

    let x = None;;
    let y = Some 1
    |};
  [%expect
    {|
    (result (
      Ok (
        ((values (
           (x ((quantifiers ()) (ty (Apply option ((Var 1))))))
           (y ((quantifiers ()) (ty (Apply option ((Var 2))))))))
         (type_declarations ((
           option (
             (shape (Variant (constructors ((None ()) (Some ((Var 0))))) (id 0)))
             (args (0))))))
         (constructors (
           (None option)
           (Some option)))
         (fields ()))
        ((
          Same_type
          (Intrinsic Int)
          (Var       2)
          "check t1 t2")))))
    |}]
;;

let%expect_test "list" =
  test_fragment
    {|
    type 'a list = | Nil | Cons of 'a * 'a list

    let x = Nil
    ;;

    let y = Cons (3, Nil)
    ;;

    let z = Cons (2, y)
    ;;
    |};
  [%expect
    {|
    (result (
      Ok (
        ((values (
           (x ((quantifiers ()) (ty (Apply list ((Var 1))))))
           (y ((quantifiers ()) (ty (Apply list ((Var 2))))))
           (z ((quantifiers ()) (ty (Apply list ((Var 4))))))))
         (type_declarations ((
           list (
             (shape (
               Variant
               (constructors (
                 (Nil ()) (Cons ((Tuple ((Var 0) (Apply list ((Var 0)))))))))
               (id 0)))
             (args (0))))))
         (constructors (
           (Cons list)
           (Nil  list)))
         (fields ()))
        ((Same_type
           (Tuple ((Intrinsic Int) (Apply list ((Var 3)))))
           (Tuple ((Var 2) (Apply list ((Var 2)))))
           "check t1 t2")
         (Same_type
           (Tuple ((Intrinsic Int) (Apply list ((Var 2)))))
           (Tuple ((Var 4) (Apply list ((Var 4)))))
           "check t1 t2")))))
    |}]
;;

let%expect_test "list and options" =
  test_fragment
    {|
    type 'a list = | Nil | Cons of 'a * 'a list
    type 'a option = | None | Some of 'a

    let x = Cons (3, Nil)
    ;;

    let hd =
      match x with
      | Cons (x, _) -> Some x
      | Nil -> None
    ;;
    |};
  [%expect
    {|
    (result (
      Ok (
        ((values (
           (hd ((quantifiers ()) (ty (Apply option ((Var 7))))))
           (x ((quantifiers ()) (ty (Apply list ((Var 2))))))))
         (type_declarations (
           (list (
             (shape (
               Variant
               (constructors (
                 (Nil ()) (Cons ((Tuple ((Var 0) (Apply list ((Var 0)))))))))
               (id 0)))
             (args (0))))
           (option (
             (shape (Variant (constructors ((None ()) (Some ((Var 1))))) (id 1)))
             (args (1))))))
         (constructors (
           (Cons list)
           (Nil  list)
           (None option)
           (Some option)))
         (fields ()))
        ((Same_type
           (Tuple ((Intrinsic Int) (Apply list ((Var 3)))))
           (Tuple ((Var 2) (Apply list ((Var 2)))))
           "check t1 t2")
         (Same_type
           (Apply list ((Var 2)))
           (Apply list ((Var 4)))
           "pattern (construct: some args)")
         (Same_type
           (Tuple ((Var 4) (Apply list ((Var 4)))))
           (Tuple (
             (Var 5)
             (Var 6)))
           "pattern (tuple)")
         (Same_type
           (Var 5)
           (Var 7)
           "check t1 t2")
         (Same_type
           (Apply list ((Var 2)))
           (Apply list ((Var 8)))
           "pattern (construct: no args)")
         (Same_type
           (Apply option ((Var 9)))
           (Apply option ((Var 7)))
           "check t1 t2")))))
    |}]
;;

let%expect_test "polymorphism" =
  test_fragment
    {|
      type 'a option = | None | Some of 'a
      type 'a list = | Nil | Cons of 'a * 'a list

      let hd = fun (l) ->
        match l with
        | Nil -> None
        | Cons (hd, _) -> Some hd
      ;;
  |};
  [%expect
    {|
    (result (
      Ok (
        ((values ((
           hd ((quantifiers ()) (ty (Fun ((Var 2)) (Apply option ((Var 4)))))))))
         (type_declarations (
           (list (
             (shape (
               Variant
               (constructors (
                 (Nil ()) (Cons ((Tuple ((Var 1) (Apply list ((Var 1)))))))))
               (id 1)))
             (args (1))))
           (option (
             (shape (Variant (constructors ((None ()) (Some ((Var 0))))) (id 0)))
             (args (0))))))
         (constructors (
           (Cons list)
           (Nil  list)
           (None option)
           (Some option)))
         (fields ()))
        ((Same_type (Var 2) (Apply list ((Var 3))) "pattern (construct: no args)")
         (Same_type
           (Var 2)
           (Apply list ((Var 5)))
           "pattern (construct: some args)")
         (Same_type
           (Tuple ((Var 5) (Apply list ((Var 5)))))
           (Tuple (
             (Var 6)
             (Var 7)))
           "pattern (tuple)")
         (Same_type
           (Apply option ((Var 8)))
           (Apply option ((Var 4)))
           "check t1 t2")
         (Same_type
           (Var 6)
           (Var 8)
           "check t1 t2")))))
    |}]
;;*)
