open! Core

let example0 =
  {|
  type int = "%int"

  let a = 10
  let b = print()
  let c = add(1,2,3,4)
  let d = (1,2,3)
  let e = (1)

  type t = {
    a : int;
    b : int;
    c : int
  }

  type 'a t = {
    a : int
  }
  |}
;;

let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Llama_frontend.Parser.program Llama_frontend.Lexer.read lexbuf in
  Llama.Pretty_print.pp_ast Format.std_formatter ast
;;

let%expect_test _ =
  parse example0;
  [%expect
    {|
    type int = "%int"
    let a = 10
    let b = print()
    let c = add(1, 2, 3, 4)
    let d = (1, 2, 3)
    let e = (1)
    type t = {a : int; b : int; c : int}
    type 'a t = {a : int}
    |}]
;;
