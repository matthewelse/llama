open! Core
open! Import
module Env = Llama_typing.Env

let parse_with_error_reporting code ~pp_ast =
  let lexbuf = Lexing.from_string code in
  Lexing.set_filename lexbuf "<example>";
  match Llama_frontend.Parser.program Llama_frontend.Lexer.read lexbuf with
  | exception Llama_frontend.Parser.Error n ->
    let error_output =
      Diagnostics.create
        ~code
        ~message:"Syntax error"
        ~error_code:[%string "E%{n#Int}"]
        ~error_offset:(Lexing.lexeme_start_p lexbuf)
        ~labels:
          { primary =
              { span = Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf
              ; message = Llama_frontend.Errors.message n |> String.rstrip
              }
          ; secondary = []
          }
        Error
    in
    Diagnostics.render error_output Format.err_formatter;
    Error ()
  | ast ->
    if pp_ast then Pretty_print.pp_ast Format.std_formatter ast;
    Ok ast
;;

let test_fragment ?(pp_ast = false) code =
  Type.Var.For_testing.reset_counter ();
  Type.Id.For_testing.reset_counter ();
  (ignore : (unit, unit) result -> unit)
  @@
  let%bind.Result ast = parse_with_error_reporting code ~pp_ast in
  let result = Llama_typing.Infer.type_ast ast in
  match result with
  | Ok env ->
    print_s [%message (env : Env.t)];
    Ok ()
  | Error { primary_location; message } ->
    let error_output =
      Diagnostics.create
        ~code
        ~message:"Type Error"
        ~error_code:[%string "EXXXX"]
        ~error_offset:(fst primary_location)
        ~labels:{ primary = { span = primary_location; message }; secondary = [] }
        Error
    in
    Diagnostics.render error_output Format.err_formatter;
    Ok ()
;;

let%expect_test "int" =
  test_fragment "let x = 1";
  [%expect
    {|
    (env (
      (values ((x ((quantifiers ()) (ty (Intrinsic Int))))))
      (type_declarations ())
      (constructors      ())
      (fields            ())))
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
    (env (
      (values (
        (x (
          (quantifiers (1))
          (ty (
            Apply ((value option) (loc (<example>:2:4 <example>:2:40))) ((Var 1))))))
        (y (
          (quantifiers ())
          (ty (
            Apply
            ((value option) (loc (<example>:2:4 <example>:2:40)))
            ((Intrinsic Int))))))))
      (type_declarations ((
        option (
          (shape (
            Variant
            (constructors (
              (((value None) (loc (<example>:2:23 <example>:2:27))) ())
              (((value Some) (loc (<example>:2:30 <example>:2:34))) ((Var 0)))))
            (id 0)))
          (args (0))
          (loc (<example>:2:4 <example>:2:40))))))
      (constructors (
        (None option)
        (Some option)))
      (fields ())))
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
    (env (
      (values (
        (x (
          (quantifiers (1))
          (ty (
            Apply ((value list) (loc (<example>:2:4 <example>:2:47))) ((Var 1))))))
        (y (
          (quantifiers ())
          (ty (
            Apply
            ((value list) (loc (<example>:2:4 <example>:2:47)))
            ((Intrinsic Int))))))
        (z (
          (quantifiers ())
          (ty (
            Apply
            ((value list) (loc (<example>:2:4 <example>:2:47)))
            ((Intrinsic Int))))))))
      (type_declarations ((
        list (
          (shape (
            Variant
            (constructors (
              (((value Nil) (loc (<example>:2:21 <example>:2:24))) ())
              (((value Cons) (loc (<example>:2:27 <example>:2:31)))
               ((
                 Tuple (
                   (Var 0)
                   (Apply
                     ((value list) (loc (<example>:2:43 <example>:2:47)))
                     ((Var 0)))))))))
            (id 0)))
          (args (0))
          (loc (<example>:2:4 <example>:2:47))))))
      (constructors (
        (Cons list)
        (Nil  list)))
      (fields ())))
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
    (env (
      (values (
        (hd (
          (quantifiers ())
          (ty (
            Apply
            ((value option) (loc (<example>:3:4 <example>:3:40)))
            ((Intrinsic Int))))))
        (x (
          (quantifiers ())
          (ty (
            Apply
            ((value list) (loc (<example>:2:4 <example>:2:47)))
            ((Intrinsic Int))))))))
      (type_declarations (
        (list (
          (shape (
            Variant
            (constructors (
              (((value Nil) (loc (<example>:2:21 <example>:2:24))) ())
              (((value Cons) (loc (<example>:2:27 <example>:2:31)))
               ((
                 Tuple (
                   (Var 0)
                   (Apply
                     ((value list) (loc (<example>:2:43 <example>:2:47)))
                     ((Var 0)))))))))
            (id 0)))
          (args (0))
          (loc (<example>:2:4 <example>:2:47))))
        (option (
          (shape (
            Variant
            (constructors (
              (((value None) (loc (<example>:3:23 <example>:3:27))) ())
              (((value Some) (loc (<example>:3:30 <example>:3:34))) ((Var 1)))))
            (id 1)))
          (args (1))
          (loc (<example>:3:4 <example>:3:40))))))
      (constructors (
        (Cons list)
        (Nil  list)
        (None option)
        (Some option)))
      (fields ())))
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
    (env (
      (values ((
        x (
          (quantifiers ())
          (ty (
            Tuple (
              (Apply ((value a) (loc (<example>:2:4 <example>:2:16))) ())
              (Apply ((value b) (loc (<example>:3:4 <example>:3:16))) ())
              (Apply ((value c) (loc (<example>:4:4 <example>:4:16))) ()))))))))
      (type_declarations (
        (a (
          (shape (
            Variant
            (constructors ((((value A) (loc (<example>:2:15 <example>:2:16))) ())))
            (id 0)))
          (args ())
          (loc (<example>:2:4 <example>:2:16))))
        (b (
          (shape (
            Variant
            (constructors ((((value B) (loc (<example>:3:15 <example>:3:16))) ())))
            (id 1)))
          (args ())
          (loc (<example>:3:4 <example>:3:16))))
        (c (
          (shape (
            Variant
            (constructors ((((value C) (loc (<example>:4:15 <example>:4:16))) ())))
            (id 2)))
          (args ())
          (loc (<example>:4:4 <example>:4:16))))))
      (constructors (
        (A a)
        (B b)
        (C c)))
      (fields ())))
    |}]
;;

let%expect_test "variables" =
  test_fragment {|
    let x = 3
    let y = x
    |};
  [%expect
    {|
    (env (
      (values (
        (x ((quantifiers ()) (ty (Intrinsic Int))))
        (y ((quantifiers ()) (ty (Intrinsic Int))))))
      (type_declarations ())
      (constructors      ())
      (fields            ())))
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
    (env (
      (values (
        (add_int (
          (quantifiers ())
          (ty (
            Fun
            ((Apply ((value int) (loc (<example>:3:22 <example>:3:25))) ())
             (Apply ((value int) (loc (<example>:3:29 <example>:3:32))) ()))
            (Apply ((value int) (loc (<example>:3:36 <example>:3:39))) ())))))
        (y (
          (quantifiers ())
          (ty (Apply ((value int) (loc (<example>:3:36 <example>:3:39))) ()))))))
      (type_declarations ((
        int (
          (shape (Alias (Intrinsic Int)))
          (args ())
          (loc (<example>:2:2 <example>:2:19))))))
      (constructors ())
      (fields       ())))
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
    (env (
      (values (
        (add_int (
          (quantifiers ())
          (ty (
            Fun
            ((Apply ((value int) (loc (<example>:3:22 <example>:3:25))) ())
             (Apply ((value int) (loc (<example>:3:29 <example>:3:32))) ()))
            (Apply ((value int) (loc (<example>:3:36 <example>:3:39))) ())))))
        (add_twice (
          (quantifiers ())
          (ty (
            Fun
            ((Apply ((value int) (loc (<example>:3:22 <example>:3:25))) ())
             (Apply ((value int) (loc (<example>:3:29 <example>:3:32))) ()))
            (Apply ((value int) (loc (<example>:3:36 <example>:3:39))) ())))))
        (y (
          (quantifiers ())
          (ty (Apply ((value int) (loc (<example>:3:36 <example>:3:39))) ()))))))
      (type_declarations ((
        int (
          (shape (Alias (Intrinsic Int)))
          (args ())
          (loc (<example>:2:2 <example>:2:19))))))
      (constructors ())
      (fields       ())))
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
  [%expect
    {|
    (env (
      (values (
        (a (
          (quantifiers ())
          (ty (
            Apply
            ((value option) (loc (<example>:2:6 <example>:2:42)))
            ((Intrinsic Int))))))
        (hd (
          (quantifiers (3))
          (ty (
            Fun
            ((
              Apply
              ((value list) (loc (<example>:7:10 <example>:7:13)))
              ((Var 3))))
            (Apply
              ((value option) (loc (<example>:2:6 <example>:2:42)))
              ((Var 3)))))))
        (x (
          (quantifiers ())
          (ty (
            Apply
            ((value option) (loc (<example>:2:6 <example>:2:42)))
            ((Intrinsic Int))))))
        (y (
          (quantifiers ())
          (ty (
            Apply
            ((value option) (loc (<example>:2:6 <example>:2:42)))
            ((
              Apply
              ((value option) (loc (<example>:2:6 <example>:2:42)))
              ((Intrinsic Int))))))))
        (z (
          (quantifiers ())
          (ty (
            Apply
            ((value list) (loc (<example>:3:6 <example>:3:49)))
            ((Intrinsic Int))))))))
      (type_declarations (
        (list (
          (shape (
            Variant
            (constructors (
              (((value Nil) (loc (<example>:3:23 <example>:3:26))) ())
              (((value Cons) (loc (<example>:3:29 <example>:3:33)))
               ((
                 Tuple (
                   (Var 1)
                   (Apply
                     ((value list) (loc (<example>:3:45 <example>:3:49)))
                     ((Var 1)))))))))
            (id 1)))
          (args (1))
          (loc (<example>:3:6 <example>:3:49))))
        (option (
          (shape (
            Variant
            (constructors (
              (((value None) (loc (<example>:2:25 <example>:2:29))) ())
              (((value Some) (loc (<example>:2:32 <example>:2:36))) ((Var 0)))))
            (id 0)))
          (args (0))
          (loc (<example>:2:6 <example>:2:42))))))
      (constructors (
        (Cons list)
        (Nil  list)
        (None option)
        (Some option)))
      (fields ())))
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

  let y = set_ref (x, (Some 1))
  let z = set_ref (x, (Some None))
  |};
  [%expect
    {|
    error[EXXXX]: Type Error
       ╭─[<example>:13:28]
    13 │   let z = set_ref (x, (Some None))
       ┆                             ^^^^ Failed to unify types (got option, expected %int)
    ───╯
    |}]
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
    {|
    error[EXXXX]: Type Error
       ╭─[<example>:12:24]
    12 │     let a = set_ref (y, Some 10) in
       ┆                         ^^^^^^^ Failed to unify types (got option, expected %int)
    ───╯
    |}]
;;

let%expect_test "don't allow different branches of a match statement to have different \
                 types"
  =
  test_fragment
    {|
  type 'a option = | None | Some of 'a
  type 'a list   = | Nil  | Cons of 'a * 'a list

  let hd_tl = fun (l) ->
    match l with
    | Nil -> (None, None)
    | Cons (x, y) -> (Some x, y)
  |};
  (* FIXME: error should look like

     {v
| Cons (x, y) -> (Some x, y)
                          ^ Types list and option are not equal.
     v}*)
  [%expect
    {|
    error[EXXXX]: Type Error
      ╭─[<example>:3:44]
    3 │   type 'a list   = | Nil  | Cons of 'a * 'a list
      ┆                                             ^^^^ Types list and option are not equal.
    ──╯
    |}]
;;
