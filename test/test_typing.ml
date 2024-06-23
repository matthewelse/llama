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
  let env, constraints = Constraints.type_ast ast |> ok_exn in
  let result = Llama_typing.Solver.solve constraints ~env in
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
  [%expect {|
    (result (
      Ok (
        (values (
          (x ((quantifiers ()) (ty (Apply option ((Var 1))))))
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
          (x ((quantifiers ()) (ty (Apply list ((Var 1))))))
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
  [%expect
    {|
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
              ((Intrinsic Int)
               (Intrinsic Int))
              (Intrinsic Int)))))
          (y ((quantifiers ()) (ty (Intrinsic Int))))))
        (type_declarations ((int ((shape (Alias (Intrinsic Int))) (args ())))))
        (constructors ())
        (fields       ()))))
    |}]
;;
