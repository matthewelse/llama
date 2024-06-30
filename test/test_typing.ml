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
          (ty (Apply ((value int) (loc (:0:-1 :0:-1))) ()))
          (constraints ())))))
      (type_declarations (
        (bool ((shape (Intrinsic Bool)) (args ()) (loc (:0:-1 :0:-1))))
        (int ((shape (Intrinsic Int)) (args ()) (loc (:0:-1 :0:-1))))
        (ref ((shape (Intrinsic Ref)) (args (0)) (loc (:0:-1 :0:-1))))
        (string ((shape (Intrinsic String)) (args ()) (loc (:0:-1 :0:-1))))))
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
          (ty (
            Apply ((value option) (loc (<example>:2:4 <example>:2:40))) ((Var 3))))
          (constraints ())))
        (y (
          (quantifiers ())
          (ty (
            Apply
            ((value option) (loc (<example>:2:4 <example>:2:40)))
            ((Apply ((value int) (loc (:0:-1 :0:-1))) ()))))
          (constraints ())))))
      (type_declarations (
        (bool ((shape (Intrinsic Bool)) (args ()) (loc (:0:-1 :0:-1))))
        (int ((shape (Intrinsic Int)) (args ()) (loc (:0:-1 :0:-1))))
        (option (
          (shape (
            Variant
            (constructors (
              (((value None) (loc (<example>:2:23 <example>:2:27))) ())
              (((value Some) (loc (<example>:2:30 <example>:2:34))) ((Var 1)))))
            (id 0)))
          (args (1))
          (loc (<example>:2:4 <example>:2:40))))
        (ref ((shape (Intrinsic Ref)) (args (0)) (loc (:0:-1 :0:-1))))
        (string ((shape (Intrinsic String)) (args ()) (loc (:0:-1 :0:-1))))))
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
          (ty (
            Apply ((value list) (loc (<example>:2:4 <example>:2:47))) ((Var 3))))
          (constraints ())))
        (y (
          (quantifiers ())
          (ty (
            Apply
            ((value list) (loc (<example>:2:4 <example>:2:47)))
            ((Apply ((value int) (loc (:0:-1 :0:-1))) ()))))
          (constraints ())))
        (z (
          (quantifiers ())
          (ty (
            Apply
            ((value list) (loc (<example>:2:4 <example>:2:47)))
            ((Apply ((value int) (loc (:0:-1 :0:-1))) ()))))
          (constraints ())))))
      (type_declarations (
        (bool ((shape (Intrinsic Bool)) (args ()) (loc (:0:-1 :0:-1))))
        (int ((shape (Intrinsic Int)) (args ()) (loc (:0:-1 :0:-1))))
        (list (
          (shape (
            Variant
            (constructors (
              (((value Nil) (loc (<example>:2:21 <example>:2:24))) ())
              (((value Cons) (loc (<example>:2:27 <example>:2:31)))
               ((
                 Tuple (
                   (Var 1)
                   (Apply
                     ((value list) (loc (<example>:2:43 <example>:2:47)))
                     ((Var 1)))))))))
            (id 0)))
          (args (1))
          (loc (<example>:2:4 <example>:2:47))))
        (ref ((shape (Intrinsic Ref)) (args (0)) (loc (:0:-1 :0:-1))))
        (string ((shape (Intrinsic String)) (args ()) (loc (:0:-1 :0:-1))))))
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
          (ty (
            Apply
            ((value option) (loc (<example>:3:4 <example>:3:40)))
            ((Apply ((value int) (loc (:0:-1 :0:-1))) ()))))
          (constraints ())))
        (x (
          (quantifiers ())
          (ty (
            Apply
            ((value list) (loc (<example>:2:4 <example>:2:47)))
            ((Apply ((value int) (loc (:0:-1 :0:-1))) ()))))
          (constraints ())))))
      (type_declarations (
        (bool ((shape (Intrinsic Bool)) (args ()) (loc (:0:-1 :0:-1))))
        (int ((shape (Intrinsic Int)) (args ()) (loc (:0:-1 :0:-1))))
        (list (
          (shape (
            Variant
            (constructors (
              (((value Nil) (loc (<example>:2:21 <example>:2:24))) ())
              (((value Cons) (loc (<example>:2:27 <example>:2:31)))
               ((
                 Tuple (
                   (Var 1)
                   (Apply
                     ((value list) (loc (<example>:2:43 <example>:2:47)))
                     ((Var 1)))))))))
            (id 0)))
          (args (1))
          (loc (<example>:2:4 <example>:2:47))))
        (option (
          (shape (
            Variant
            (constructors (
              (((value None) (loc (<example>:3:23 <example>:3:27))) ())
              (((value Some) (loc (<example>:3:30 <example>:3:34))) ((Var 2)))))
            (id 1)))
          (args (2))
          (loc (<example>:3:4 <example>:3:40))))
        (ref ((shape (Intrinsic Ref)) (args (0)) (loc (:0:-1 :0:-1))))
        (string ((shape (Intrinsic String)) (args ()) (loc (:0:-1 :0:-1))))))
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
          (ty (
            Tuple (
              (Apply ((value a) (loc (<example>:2:4 <example>:2:16))) ())
              (Apply ((value b) (loc (<example>:3:4 <example>:3:16))) ())
              (Apply ((value c) (loc (<example>:4:4 <example>:4:16))) ()))))
          (constraints ())))))
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
        (bool ((shape (Intrinsic Bool)) (args ()) (loc (:0:-1 :0:-1))))
        (c (
          (shape (
            Variant
            (constructors ((((value C) (loc (<example>:4:15 <example>:4:16))) ())))
            (id 2)))
          (args ())
          (loc (<example>:4:4 <example>:4:16))))
        (int ((shape (Intrinsic Int)) (args ()) (loc (:0:-1 :0:-1))))
        (ref ((shape (Intrinsic Ref)) (args (0)) (loc (:0:-1 :0:-1))))
        (string ((shape (Intrinsic String)) (args ()) (loc (:0:-1 :0:-1))))))
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
          (ty (Apply ((value int) (loc (:0:-1 :0:-1))) ()))
          (constraints ())))
        (y (
          (quantifiers ())
          (ty (Apply ((value int) (loc (:0:-1 :0:-1))) ()))
          (constraints ())))))
      (type_declarations (
        (bool ((shape (Intrinsic Bool)) (args ()) (loc (:0:-1 :0:-1))))
        (int ((shape (Intrinsic Int)) (args ()) (loc (:0:-1 :0:-1))))
        (ref ((shape (Intrinsic Ref)) (args (0)) (loc (:0:-1 :0:-1))))
        (string ((shape (Intrinsic String)) (args ()) (loc (:0:-1 :0:-1))))))
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
          (ty (
            Fun
            ((Apply ((value int) (loc (<example>:2:22 <example>:2:25))) ())
             (Apply ((value int) (loc (<example>:2:29 <example>:2:32))) ()))
            (Apply ((value int) (loc (<example>:2:36 <example>:2:39))) ())))
          (constraints ())))
        (y (
          (quantifiers ())
          (ty (Apply ((value int) (loc (<example>:2:36 <example>:2:39))) ()))
          (constraints ())))))
      (type_declarations (
        (bool ((shape (Intrinsic Bool)) (args ()) (loc (:0:-1 :0:-1))))
        (int ((shape (Intrinsic Int)) (args ()) (loc (:0:-1 :0:-1))))
        (ref ((shape (Intrinsic Ref)) (args (0)) (loc (:0:-1 :0:-1))))
        (string ((shape (Intrinsic String)) (args ()) (loc (:0:-1 :0:-1))))))
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
          (ty (
            Fun
            ((Apply ((value int) (loc (<example>:2:22 <example>:2:25))) ())
             (Apply ((value int) (loc (<example>:2:29 <example>:2:32))) ()))
            (Apply ((value int) (loc (<example>:2:36 <example>:2:39))) ())))
          (constraints ())))
        (int_add (
          (quantifiers ())
          (ty (
            Fun
            ((Apply ((value int) (loc (<example>:2:22 <example>:2:25))) ())
             (Apply ((value int) (loc (<example>:2:29 <example>:2:32))) ()))
            (Apply ((value int) (loc (<example>:2:36 <example>:2:39))) ())))
          (constraints ())))
        (y (
          (quantifiers ())
          (ty (Apply ((value int) (loc (<example>:2:36 <example>:2:39))) ()))
          (constraints ())))))
      (type_declarations (
        (bool ((shape (Intrinsic Bool)) (args ()) (loc (:0:-1 :0:-1))))
        (int ((shape (Intrinsic Int)) (args ()) (loc (:0:-1 :0:-1))))
        (ref ((shape (Intrinsic Ref)) (args (0)) (loc (:0:-1 :0:-1))))
        (string ((shape (Intrinsic String)) (args ()) (loc (:0:-1 :0:-1))))))
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
          (ty (
            Apply
            ((value option) (loc (<example>:2:6 <example>:2:42)))
            ((Apply ((value int) (loc (:0:-1 :0:-1))) ()))))
          (constraints ())))
        (hd (
          (quantifiers (5))
          (ty (
            Fun
            ((
              Apply
              ((value list) (loc (<example>:7:10 <example>:7:13)))
              ((Var 5))))
            (Apply
              ((value option) (loc (<example>:2:6 <example>:2:42)))
              ((Var 5)))))
          (constraints ())))
        (x (
          (quantifiers ())
          (ty (
            Apply
            ((value option) (loc (<example>:2:6 <example>:2:42)))
            ((Apply ((value int) (loc (:0:-1 :0:-1))) ()))))
          (constraints ())))
        (y (
          (quantifiers ())
          (ty (
            Apply
            ((value option) (loc (<example>:2:6 <example>:2:42)))
            ((
              Apply
              ((value option) (loc (<example>:2:6 <example>:2:42)))
              ((Apply ((value int) (loc (:0:-1 :0:-1))) ()))))))
          (constraints ())))
        (z (
          (quantifiers ())
          (ty (
            Apply
            ((value list) (loc (<example>:3:6 <example>:3:49)))
            ((Apply ((value int) (loc (:0:-1 :0:-1))) ()))))
          (constraints ())))))
      (type_declarations (
        (bool ((shape (Intrinsic Bool)) (args ()) (loc (:0:-1 :0:-1))))
        (int ((shape (Intrinsic Int)) (args ()) (loc (:0:-1 :0:-1))))
        (list (
          (shape (
            Variant
            (constructors (
              (((value Nil) (loc (<example>:3:23 <example>:3:26))) ())
              (((value Cons) (loc (<example>:3:29 <example>:3:33)))
               ((
                 Tuple (
                   (Var 2)
                   (Apply
                     ((value list) (loc (<example>:3:45 <example>:3:49)))
                     ((Var 2)))))))))
            (id 1)))
          (args (2))
          (loc (<example>:3:6 <example>:3:49))))
        (option (
          (shape (
            Variant
            (constructors (
              (((value None) (loc (<example>:2:25 <example>:2:29))) ())
              (((value Some) (loc (<example>:2:32 <example>:2:36))) ((Var 1)))))
            (id 0)))
          (args (1))
          (loc (<example>:2:6 <example>:2:42))))
        (ref ((shape (Intrinsic Ref)) (args (0)) (loc (:0:-1 :0:-1))))
        (string ((shape (Intrinsic String)) (args ()) (loc (:0:-1 :0:-1))))))
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
    error[EXXXX]: Type Error
       ╭─[<example>:2:2]
     2 │   type 'a option = | None | Some of 'a
       ┆   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ Types option and int are not equal.
    ───╯
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
    error[EXXXX]: Type Error
       ╭─[<example>:2:2]
     2 │   type 'a option = | None | Some of 'a
       ┆   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ Types option and int are not equal.
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
          (ty (
            Fun
            ((Apply ((value int) (loc (<example>:4:22 <example>:4:25))) ())
             (Apply ((value int) (loc (<example>:4:29 <example>:4:32))) ()))
            (Apply ((value int) (loc (<example>:4:36 <example>:4:39))) ())))
          (constraints ())))
        (len (
          (quantifiers (4))
          (ty (
            Fun
            ((Apply ((value list) (loc (<example>:8:6 <example>:8:9))) ((Var 4))))
            (Apply ((value int) (loc (:0:-1 :0:-1))) ())))
          (constraints ())))))
      (type_declarations (
        (bool ((shape (Intrinsic Bool)) (args ()) (loc (:0:-1 :0:-1))))
        (int ((shape (Intrinsic Int)) (args ()) (loc (:0:-1 :0:-1))))
        (list (
          (shape (
            Variant
            (constructors (
              (((value Nil) (loc (<example>:2:19 <example>:2:22))) ())
              (((value Cons) (loc (<example>:2:25 <example>:2:29)))
               ((
                 Tuple (
                   (Var 1)
                   (Apply
                     ((value list) (loc (<example>:2:41 <example>:2:45)))
                     ((Var 1)))))))))
            (id 0)))
          (args (1))
          (loc (<example>:2:2 <example>:2:45))))
        (ref ((shape (Intrinsic Ref)) (args (0)) (loc (:0:-1 :0:-1))))
        (string ((shape (Intrinsic String)) (args ()) (loc (:0:-1 :0:-1))))))
      (constructors (
        (Cons list)
        (Nil  list)))
      (fields                     ())
      (type_classes               ())
      (type_class_implementations ())))
    |}]
;;
