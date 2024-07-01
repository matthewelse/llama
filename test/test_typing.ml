open! Core
open! Import
open Helpers

let%expect_test "int" =
  test_fragment "let x = 1";
  [%expect
    {|
    (env (
      (values ((
        x (
          (quantifiers ())
          (body (Apply (((int Built_in) ()) ())))
          (constraints ())))))
      (type_declarations (
        (bool ((shape (Intrinsic Bool)) (args ()) (loc Built_in)))
        (int ((shape (Intrinsic Int)) (args ()) (loc Built_in)))
        (ref ((shape (Intrinsic Ref)) (args (0)) (loc Built_in)))
        (string ((shape (Intrinsic String)) (args ()) (loc Built_in)))))
      (constructors               ())
      (fields                     ())
      (type_classes               ())
      (type_class_implementations ())))
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
          (quantifiers (3))
          (body (
            Apply (
              ((option (Position (<example>:2:4 <example>:2:40))) ((Var (3 ()))))
              ())))
          (constraints ())))
        (y (
          (quantifiers ())
          (body (
            Apply (
              ((option (Position (<example>:2:4 <example>:2:40)))
               ((Apply (((int Built_in) ()) ()))))
              ())))
          (constraints ())))))
      (type_declarations (
        (bool ((shape (Intrinsic Bool)) (args ()) (loc Built_in)))
        (int ((shape (Intrinsic Int)) (args ()) (loc Built_in)))
        (option (
          (shape (
            Variant
            (constructors (
              ((None (<example>:2:23 <example>:2:27)) ())
              ((Some (<example>:2:30 <example>:2:34)) ((Var (1 ()))))))
            (id 0)))
          (args (1))
          (loc (Position (<example>:2:4 <example>:2:40)))))
        (ref ((shape (Intrinsic Ref)) (args (0)) (loc Built_in)))
        (string ((shape (Intrinsic String)) (args ()) (loc Built_in)))))
      (constructors (
        (None option)
        (Some option)))
      (fields                     ())
      (type_classes               ())
      (type_class_implementations ())))
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
          (quantifiers (3))
          (body (
            Apply (
              ((list (Position (<example>:2:4 <example>:2:47))) ((Var (3 ()))))
              ())))
          (constraints ())))
        (y (
          (quantifiers ())
          (body (
            Apply (
              ((list (Position (<example>:2:4 <example>:2:47)))
               ((Apply (((int Built_in) ()) ()))))
              ())))
          (constraints ())))
        (z (
          (quantifiers ())
          (body (
            Apply (
              ((list (Position (<example>:2:4 <example>:2:47)))
               ((Apply (((int Built_in) ()) ()))))
              ())))
          (constraints ())))))
      (type_declarations (
        (bool ((shape (Intrinsic Bool)) (args ()) (loc Built_in)))
        (int ((shape (Intrinsic Int)) (args ()) (loc Built_in)))
        (list (
          (shape (
            Variant
            (constructors (
              ((Nil (<example>:2:21 <example>:2:24)) ())
              ((Cons (<example>:2:27 <example>:2:31))
               ((
                 Tuple (
                   ((Var (1 ()))
                    (Apply (
                      ((list (Position (<example>:2:43 <example>:2:47)))
                       ((Var (1 ()))))
                      ())))
                   ()))))))
            (id 0)))
          (args (1))
          (loc (Position (<example>:2:4 <example>:2:47)))))
        (ref ((shape (Intrinsic Ref)) (args (0)) (loc Built_in)))
        (string ((shape (Intrinsic String)) (args ()) (loc Built_in)))))
      (constructors (
        (Cons list)
        (Nil  list)))
      (fields                     ())
      (type_classes               ())
      (type_class_implementations ())))
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
          (body (
            Apply (
              ((option (Position (<example>:3:4 <example>:3:40)))
               ((Apply (((int Built_in) ()) ()))))
              ())))
          (constraints ())))
        (x (
          (quantifiers ())
          (body (
            Apply (
              ((list (Position (<example>:2:4 <example>:2:47)))
               ((Apply (((int Built_in) ()) ()))))
              ())))
          (constraints ())))))
      (type_declarations (
        (bool ((shape (Intrinsic Bool)) (args ()) (loc Built_in)))
        (int ((shape (Intrinsic Int)) (args ()) (loc Built_in)))
        (list (
          (shape (
            Variant
            (constructors (
              ((Nil (<example>:2:21 <example>:2:24)) ())
              ((Cons (<example>:2:27 <example>:2:31))
               ((
                 Tuple (
                   ((Var (1 ()))
                    (Apply (
                      ((list (Position (<example>:2:43 <example>:2:47)))
                       ((Var (1 ()))))
                      ())))
                   ()))))))
            (id 0)))
          (args (1))
          (loc (Position (<example>:2:4 <example>:2:47)))))
        (option (
          (shape (
            Variant
            (constructors (
              ((None (<example>:3:23 <example>:3:27)) ())
              ((Some (<example>:3:30 <example>:3:34)) ((Var (2 ()))))))
            (id 1)))
          (args (2))
          (loc (Position (<example>:3:4 <example>:3:40)))))
        (ref ((shape (Intrinsic Ref)) (args (0)) (loc Built_in)))
        (string ((shape (Intrinsic String)) (args ()) (loc Built_in)))))
      (constructors (
        (Cons list)
        (Nil  list)
        (None option)
        (Some option)))
      (fields                     ())
      (type_classes               ())
      (type_class_implementations ())))
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
          (body (
            Tuple (
              ((Apply (((a (Position (<example>:2:4 <example>:2:16))) ()) ()))
               (Apply (((b (Position (<example>:3:4 <example>:3:16))) ()) ()))
               (Apply (((c (Position (<example>:4:4 <example>:4:16))) ()) ())))
              ())))
          (constraints ())))))
      (type_declarations (
        (a (
          (shape (
            Variant
            (constructors (((A (<example>:2:15 <example>:2:16)) ())))
            (id 0)))
          (args ())
          (loc (Position (<example>:2:4 <example>:2:16)))))
        (b (
          (shape (
            Variant
            (constructors (((B (<example>:3:15 <example>:3:16)) ())))
            (id 1)))
          (args ())
          (loc (Position (<example>:3:4 <example>:3:16)))))
        (bool ((shape (Intrinsic Bool)) (args ()) (loc Built_in)))
        (c (
          (shape (
            Variant
            (constructors (((C (<example>:4:15 <example>:4:16)) ())))
            (id 2)))
          (args ())
          (loc (Position (<example>:4:4 <example>:4:16)))))
        (int ((shape (Intrinsic Int)) (args ()) (loc Built_in)))
        (ref ((shape (Intrinsic Ref)) (args (0)) (loc Built_in)))
        (string ((shape (Intrinsic String)) (args ()) (loc Built_in)))))
      (constructors (
        (A a)
        (B b)
        (C c)))
      (fields                     ())
      (type_classes               ())
      (type_class_implementations ())))
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
        (x (
          (quantifiers ())
          (body (Apply (((int Built_in) ()) ())))
          (constraints ())))
        (y (
          (quantifiers ())
          (body (Apply (((int Built_in) ()) ())))
          (constraints ())))))
      (type_declarations (
        (bool ((shape (Intrinsic Bool)) (args ()) (loc Built_in)))
        (int ((shape (Intrinsic Int)) (args ()) (loc Built_in)))
        (ref ((shape (Intrinsic Ref)) (args (0)) (loc Built_in)))
        (string ((shape (Intrinsic String)) (args ()) (loc Built_in)))))
      (constructors               ())
      (fields                     ())
      (type_classes               ())
      (type_class_implementations ())))
    |}]
;;

let%expect_test "let _ = _ in _" =
  test_fragment
    {|
  intrinsic int_add : int -> int -> int = "%int_add"

  let y =
    let z = 10 in
    int_add (z, z)
  |};
  [%expect
    {|
    (env (
      (values (
        (int_add (
          (quantifiers ())
          (body (
            Fun (
              (((Apply (((int (Position (<example>:2:22 <example>:2:25))) ()) ()))
                (Apply (((int (Position (<example>:2:29 <example>:2:32))) ()) ())))
               (Apply (((int (Position (<example>:2:36 <example>:2:39))) ()) ())))
              ())))
          (constraints ())))
        (y (
          (quantifiers ())
          (body (Apply (((int (Position (<example>:2:36 <example>:2:39))) ()) ())))
          (constraints ())))))
      (type_declarations (
        (bool ((shape (Intrinsic Bool)) (args ()) (loc Built_in)))
        (int ((shape (Intrinsic Int)) (args ()) (loc Built_in)))
        (ref ((shape (Intrinsic Ref)) (args (0)) (loc Built_in)))
        (string ((shape (Intrinsic String)) (args ()) (loc Built_in)))))
      (constructors               ())
      (fields                     ())
      (type_classes               ())
      (type_class_implementations ())))
    |}]
;;

let%expect_test "lambdas" =
  test_fragment
    {|
  intrinsic int_add : int -> int -> int = "%int_add"

  let add_twice = fun(x,y) ->
    int_add (int_add (x, y), int_add (x, y))
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
        (add_twice (
          (quantifiers ())
          (body (
            Fun (
              (((Apply (((int (Position (<example>:2:22 <example>:2:25))) ()) ()))
                (Apply (((int (Position (<example>:2:29 <example>:2:32))) ()) ())))
               (Apply (((int (Position (<example>:2:36 <example>:2:39))) ()) ())))
              ())))
          (constraints ())))
        (int_add (
          (quantifiers ())
          (body (
            Fun (
              (((Apply (((int (Position (<example>:2:22 <example>:2:25))) ()) ()))
                (Apply (((int (Position (<example>:2:29 <example>:2:32))) ()) ())))
               (Apply (((int (Position (<example>:2:36 <example>:2:39))) ()) ())))
              ())))
          (constraints ())))
        (y (
          (quantifiers ())
          (body (Apply (((int (Position (<example>:2:36 <example>:2:39))) ()) ())))
          (constraints ())))))
      (type_declarations (
        (bool ((shape (Intrinsic Bool)) (args ()) (loc Built_in)))
        (int ((shape (Intrinsic Int)) (args ()) (loc Built_in)))
        (ref ((shape (Intrinsic Ref)) (args (0)) (loc Built_in)))
        (string ((shape (Intrinsic String)) (args ()) (loc Built_in)))))
      (constructors               ())
      (fields                     ())
      (type_classes               ())
      (type_class_implementations ())))
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
          (body (
            Apply (
              ((option (Position (<example>:2:6 <example>:2:42)))
               ((Apply (((int Built_in) ()) ()))))
              ())))
          (constraints ())))
        (hd (
          (quantifiers (5))
          (body (
            Fun (
              (((
                 Apply (
                   ((list (Position (<example>:7:10 <example>:7:13)))
                    ((Var (5 ()))))
                   ())))
               (Apply (
                 ((option (Position (<example>:2:6 <example>:2:42)))
                  ((Var (5 ()))))
                 ())))
              ())))
          (constraints ())))
        (x (
          (quantifiers ())
          (body (
            Apply (
              ((option (Position (<example>:2:6 <example>:2:42)))
               ((Apply (((int Built_in) ()) ()))))
              ())))
          (constraints ())))
        (y (
          (quantifiers ())
          (body (
            Apply (
              ((option (Position (<example>:2:6 <example>:2:42)))
               ((
                 Apply (
                   ((option (Position (<example>:2:6 <example>:2:42)))
                    ((Apply (((int Built_in) ()) ()))))
                   ()))))
              ())))
          (constraints ())))
        (z (
          (quantifiers ())
          (body (
            Apply (
              ((list (Position (<example>:3:6 <example>:3:49)))
               ((Apply (((int Built_in) ()) ()))))
              ())))
          (constraints ())))))
      (type_declarations (
        (bool ((shape (Intrinsic Bool)) (args ()) (loc Built_in)))
        (int ((shape (Intrinsic Int)) (args ()) (loc Built_in)))
        (list (
          (shape (
            Variant
            (constructors (
              ((Nil (<example>:3:23 <example>:3:26)) ())
              ((Cons (<example>:3:29 <example>:3:33))
               ((
                 Tuple (
                   ((Var (2 ()))
                    (Apply (
                      ((list (Position (<example>:3:45 <example>:3:49)))
                       ((Var (2 ()))))
                      ())))
                   ()))))))
            (id 1)))
          (args (2))
          (loc (Position (<example>:3:6 <example>:3:49)))))
        (option (
          (shape (
            Variant
            (constructors (
              ((None (<example>:2:25 <example>:2:29)) ())
              ((Some (<example>:2:32 <example>:2:36)) ((Var (1 ()))))))
            (id 0)))
          (args (1))
          (loc (Position (<example>:2:6 <example>:2:42)))))
        (ref ((shape (Intrinsic Ref)) (args (0)) (loc Built_in)))
        (string ((shape (Intrinsic String)) (args ()) (loc Built_in)))))
      (constructors (
        (Cons list)
        (Nil  list)
        (None option)
        (Some option)))
      (fields                     ())
      (type_classes               ())
      (type_class_implementations ())))
    |}]
;;

let%expect_test "value restriction" =
  test_fragment
    {|
  type 'a option = | None | Some of 'a

  intrinsic ref_make : 'a -> 'a ref = "%ref_make"
  intrinsic ref_set : 'a ref -> 'a -> unit = "%ref_set"

  let x = ref_make (None)

  let y = ref_set (x, (Some 1))
  let z = ref_set (x, (Some None))
  |};
  [%expect
    {|
    ￫ error[E002]
    10 |   let z = ref_set (x, (Some None))
       ^ Types option and int are not equal.
    |}]
;;

let%expect_test "don't allow a ref to be set to two different types" =
  test_fragment
    {|
  type 'a option = | None | Some of 'a

  intrinsic ref_make : 'a -> 'a ref = "%ref_make"
  intrinsic ref_set : 'a ref -> 'a -> unit = "%ref_set"

  let x = fun () ->
    let y = ref_make (None) in
    let a = ref_set (y, Some 10) in
    let b = ref_set (y, 10) in
    ()
  ;;
  |};
  [%expect
    {|
    ￫ error[E002]
    9 |     let a = ref_set (y, Some 10) in
      ^ Types option and int are not equal.
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
    ￫ error[E002]
    8 |     | Cons (x, y) -> (Some x, y)
      ^ Types list and option are not equal.
    |}]
;;

let%expect_test "recursive functions" =
  test_fragment
    {|
  type 'a list = | Nil | Cons of 'a * 'a list

  intrinsic int_add : int -> int -> int = "%int_add"

  let len = fun (x) ->
    match x with
    | Nil -> 0
    | Cons (_, tl) -> int_add (len (tl), 1)
  |};
  [%expect
    {|
    (env (
      (values (
        (int_add (
          (quantifiers ())
          (body (
            Fun (
              (((Apply (((int (Position (<example>:4:22 <example>:4:25))) ()) ()))
                (Apply (((int (Position (<example>:4:29 <example>:4:32))) ()) ())))
               (Apply (((int (Position (<example>:4:36 <example>:4:39))) ()) ())))
              ())))
          (constraints ())))
        (len (
          (quantifiers (4))
          (body (
            Fun (
              (((
                 Apply (
                   ((list (Position (<example>:8:6 <example>:8:9)))
                    ((Var (4 ()))))
                   ())))
               (Apply (((int Built_in) ()) ())))
              ())))
          (constraints ())))))
      (type_declarations (
        (bool ((shape (Intrinsic Bool)) (args ()) (loc Built_in)))
        (int ((shape (Intrinsic Int)) (args ()) (loc Built_in)))
        (list (
          (shape (
            Variant
            (constructors (
              ((Nil (<example>:2:19 <example>:2:22)) ())
              ((Cons (<example>:2:25 <example>:2:29))
               ((
                 Tuple (
                   ((Var (1 ()))
                    (Apply (
                      ((list (Position (<example>:2:41 <example>:2:45)))
                       ((Var (1 ()))))
                      ())))
                   ()))))))
            (id 0)))
          (args (1))
          (loc (Position (<example>:2:2 <example>:2:45)))))
        (ref ((shape (Intrinsic Ref)) (args (0)) (loc Built_in)))
        (string ((shape (Intrinsic String)) (args ()) (loc Built_in)))))
      (constructors (
        (Cons list)
        (Nil  list)))
      (fields                     ())
      (type_classes               ())
      (type_class_implementations ())))
    |}]
;;
