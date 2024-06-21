open! Core
open! Import

let example0 =
  {|
  type int = "%int"

  intrinsic add_int : int -> int -> int = "%add_int"

  let a = 10
  let c = add_int(1,2)
  let d = (1,2,3)
  let e = (1)

  type t = {
    a : int;
    b : int;
    c : int
  }

  type 'a t2 = {
    d : int
  }

  type 'a option =
    | None
    | Some of 'a

  let x = Some 1
  let y = Some (Some (Some None))
  let z = { d = 10 }

  type 'a list = | Nil | Cons of 'a * 'a list

  let l = Cons(3, Nil)

  let ll = Cons(1, Cons(2, Cons(3, Nil)))
  |}
;;

let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Llama_frontend.Parser.program Llama_frontend.Lexer.read lexbuf in
  Pretty_print.pp_ast Format.std_formatter ast;
  ast
;;

let%expect_test _ =
  let ast = parse example0 in
  [%expect
    {|
    type int = "%int"
    intrinsic add_int : int -> int -> int = "%add_int"
    let a = 10
    let c = add_int(1, 2)
    let d = (1, 2, 3)
    let e = 1
    type t = {a : int; b : int; c : int}
    type 'a t2 = {d : int}
    type 'a option = | None | Some of 'a
    let x = Some (1)
    let y = Some (Some (Some (None)))
    let z = {d = 10}
    type 'a list = | Nil | Cons of ('a * ('a) list)
    let l = Cons ((3, Nil))
    let ll = Cons ((1, Cons ((2, Cons ((3, Nil))))))
    |}];
  let result = Infer.type_ast ast |> ok_exn in
  print_s [%message (result : Infer.Env.t)];
  [%expect
    {|
    (result (
      (values (
        (a ((quantifiers ()) (ty (Intrinsic Int))))
        (add_int (
          (quantifiers ())
          (ty (
            Fun
            ((Apply int ())
             (Apply int ()))
            (Apply int ())))))
        (c ((quantifiers ()) (ty (Apply int ()))))
        (d (
          (quantifiers ())
          (ty (
            Tuple (
              (Intrinsic Int)
              (Intrinsic Int)
              (Intrinsic Int))))))
        (e ((quantifiers ()) (ty (Intrinsic Int))))
        (l ((quantifiers ()) (ty (Apply list ((Intrinsic Int))))))
        (ll ((quantifiers ()) (ty (Apply list ((Intrinsic Int))))))
        (x ((quantifiers ()) (ty (Apply option ((Intrinsic Int))))))
        (y (
          (quantifiers (7))
          (ty (
            Apply option ((
              Apply option ((Apply option ((Apply option ((Var 7))))))))))))
        (z ((quantifiers (8)) (ty (Apply t2 ((Var 8))))))))
      (type_declarations (
        (int ((shape (Alias (Intrinsic Int))) (args ())))
        (list (
          (shape (
            Variant
            (constructors (
              (Nil ()) (Cons ((Tuple ((Var 9) (Apply list ((Var 9)))))))))
            (id 3)))
          (args (9))))
        (option (
          (shape (Variant (constructors ((None ()) (Some ((Var 2))))) (id 2)))
          (args (2))))
        (t (
          (shape (
            Record
            (fields (
              (a (Apply int ()))
              (b (Apply int ()))
              (c (Apply int ()))))
            (id 0)))
          (args ())))
        (t2 ((shape (Record (fields ((d (Apply int ())))) (id 1))) (args (1))))))
      (constructors (
        (Cons list)
        (Nil  list)
        (None option)
        (Some option)))
      (fields (
        (a t)
        (b t)
        (c t)
        (d t2)))))
    |}]
;;
