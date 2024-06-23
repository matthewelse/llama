open! Core
open! Import
module Env = Llama_typing.Env

let test_fragment ?(pp_ast = false) code =
  Type.Var.For_testing.reset_counter ();
  Type.Id.For_testing.reset_counter ();
  let ast =
    let lexbuf = Lexing.from_string code in
    let ast = Llama_frontend.Parser.program Llama_frontend.Lexer.read lexbuf in
    if pp_ast then Pretty_print.pp_ast Format.std_formatter ast;
    ast
  in
  let result = Llama_typing.Infer.type_ast ast in
  print_s [%message (result : Env.t Or_error.t)]
;;

let%expect_test "int" =
  test_fragment "let x = 1";
  [%expect
    {|
    (result (
      Ok (
        (values ((x ((quantifiers ()) (ty (Intrinsic Int))))))
        (type_declarations ())
        (constructors      ())
        (fields            ()))))
    |}]
;;

let%expect_test "option" =
  test_fragment
    {|
    type 'a option = | None | Some of 'a

    let x = None
    ;;

    let y = Some 1
    ;;
    |};
  [%expect
    {|
    (result (
      Ok (
        (values (
          (x ((quantifiers (1)) (ty (Apply option ((Var 1))))))
          (y ((quantifiers ()) (ty (Apply option ((Intrinsic Int))))))))
        (type_declarations ((
          option (
            (shape (Variant (constructors ((None ()) (Some ((Var 0))))) (id 0)))
            (args (0))))))
        (constructors (
          (None option)
          (Some option)))
        (fields ()))))
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
        (values (
          (x ((quantifiers (1)) (ty (Apply list ((Var 1))))))
          (y ((quantifiers ()) (ty (Apply list ((Intrinsic Int))))))
          (z ((quantifiers ()) (ty (Apply list ((Intrinsic Int))))))))
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
        (fields ()))))
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
    |};
  [%expect {|
    (result (
      Ok (
        (values (
          (hd ((quantifiers ()) (ty (Apply option ((Intrinsic Int))))))
          (x ((quantifiers ()) (ty (Apply list ((Intrinsic Int))))))))
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
        (fields ()))))
    |}]
;;

let%expect_test "tuple" =
  test_fragment
    {|
    type a = | A
    type b = | B
    type c = | C

    let x = (A, B, C)
    |};
  [%expect
    {|
    (result (
      Ok (
        (values ((
          x (
            (quantifiers ())
            (ty (
              Tuple (
                (Apply a ())
                (Apply b ())
                (Apply c ()))))))))
        (type_declarations (
          (a ((shape (Variant (constructors ((A ()))) (id 0))) (args ())))
          (b ((shape (Variant (constructors ((B ()))) (id 1))) (args ())))
          (c ((shape (Variant (constructors ((C ()))) (id 2))) (args ())))))
        (constructors (
          (A a)
          (B b)
          (C c)))
        (fields ()))))
    |}]
;;

let%expect_test "variables" =
  test_fragment {|
    let x = 3
    let y = x
    |};
  [%expect
    {|
    (result (
      Ok (
        (values (
          (x ((quantifiers ()) (ty (Intrinsic Int))))
          (y ((quantifiers ()) (ty (Intrinsic Int))))))
        (type_declarations ())
        (constructors      ())
        (fields            ()))))
    |}]
;;

let%expect_test "let _ = _ in _" =
  test_fragment
    {|
  type int = "%int"
  intrinsic add_int : int -> int -> int = "%add_int"

  let y =
    let z = 10 in
    add_int (z, z)
  |};
  [%expect
    {|
    (result (
      Ok (
        (values (
          (add_int (
            (quantifiers ())
            (ty (
              Fun
              ((Apply int ())
               (Apply int ()))
              (Apply int ())))))
          (y ((quantifiers ()) (ty (Apply int ()))))))
        (type_declarations ((int ((shape (Alias (Intrinsic Int))) (args ())))))
        (constructors ())
        (fields       ()))))
    |}]
;;

let%expect_test "lambdas" =
  test_fragment
    {|
  type int = "%int"
  intrinsic add_int : int -> int -> int = "%add_int"

  let add_twice = fun(x,y) ->
    add_int (add_int (x, y), add_int (x, y))
  ;;

  let y =
    let z = 10 in
    add_twice (z, z)
  ;;
  |};
  [%expect
    {|
    (result (
      Ok (
        (values (
          (add_int (
            (quantifiers ())
            (ty (
              Fun
              ((Apply int ())
               (Apply int ()))
              (Apply int ())))))
          (add_twice (
            (quantifiers ())
            (ty (
              Fun
              ((Apply int ())
               (Apply int ()))
              (Apply int ())))))
          (y ((quantifiers ()) (ty (Apply int ()))))))
        (type_declarations ((int ((shape (Alias (Intrinsic Int))) (args ())))))
        (constructors ())
        (fields       ()))))
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

      let x = hd (Cons (3, Nil))
      ;;

      let y = hd (Cons (Some 3, Nil))
      ;;

      let z = (Cons (3, Nil))
      ;;

      let a = hd (z)
      ;;
  |};
  [%expect {|
    (result (
      Ok (
        (values (
          (a ((quantifiers ()) (ty (Apply option ((Intrinsic Int))))))
          (hd (
            (quantifiers (3))
            (ty (Fun ((Apply list ((Var 3)))) (Apply option ((Var 3)))))))
          (x ((quantifiers ()) (ty (Apply option ((Intrinsic Int))))))
          (y (
            (quantifiers ())
            (ty (Apply option ((Apply option ((Intrinsic Int))))))))
          (z ((quantifiers ()) (ty (Apply list ((Intrinsic Int))))))))
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
        (fields ()))))
    |}]
;;

let%expect_test "value restriction" =
  test_fragment
    {|
  type 'a option = | None | Some of 'a

  type unit = | Unit
  type 'a ref = "%ref"

  intrinsic make_ref : 'a -> 'a ref = "%make_ref"
  intrinsic set_ref : 'a ref -> 'a -> unit = "%set_ref"

  let x = make_ref (None)
  |};
  [%expect {| (result (Error "Value restriction")) |}]
;;

let%expect_test "don't allow a ref to be set to two different types" =
  test_fragment
    {|
  type 'a option = | None | Some of 'a

  type unit = | Unit
  type 'a ref = "%ref"

  intrinsic make_ref : 'a -> 'a ref = "%make_ref"
  intrinsic set_ref : 'a ref -> 'a -> unit = "%set_ref"

  let x = fun () ->
    let y = make_ref (None) in
    let a = set_ref (y, Some 10) in
    let b = set_ref (y, 10) in
    Unit
  ;;
  |};
  [%expect
    {| (result (Error "Failed to unify types (got option, expected %int)")) |}]
;;
