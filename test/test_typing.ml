open! Core
open! Import

let test_fragment ?(pp_ast = false) code =
  Type.Var.For_testing.reset_counter ();
  Type.Id.For_testing.reset_counter ();
  let ast =
    let lexbuf = Lexing.from_string code in
    let ast = Llama_frontend.Parser.program Llama_frontend.Lexer.read lexbuf in
    if pp_ast then Pretty_print.pp_ast Format.std_formatter ast;
    ast
  in
  let%tydi { values; _ } = Infer.type_ast ast |> ok_exn in
  print_s [%message (values : Type.Poly.t Ident.Map.t)]
;;

let%expect_test "int" =
  test_fragment "let x = 1";
  [%expect {| (values ((x ((quantifiers ()) (ty (Intrinsic Int)))))) |}]
;;

let%expect_test "string" =
  test_fragment {|let x = "hello, world"|};
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)
  (Failure "Unexpected char: \"")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Llama_frontend__Parser._menhir_run_052 in file "frontend/parser.ml", line 1287, characters 25-53
  Called from Test_llama__Test_typing.test_fragment in file "test/test_typing.ml", line 9, characters 14-76
  Called from Test_llama__Test_typing.(fun) in file "test/test_typing.ml", line 23, characters 2-42
  Called from Ppx_expect_runtime__Test_block.Configured.dump_backtrace in file "runtime/test_block.ml", line 142, characters 10-28
  |}]
;;

let%expect_test "option" =
  test_fragment
    {|
    type 'a option = | None | Some of 'a

    let x = None
    let y = Some 1
    |};
  (* FIXME melse: this shouldn't have quantifiers (value restriction) *)
  [%expect
    {|
    (values (
      (x ((quantifiers (1)) (ty (Apply option ((Var 1))))))
      (y ((quantifiers ()) (ty (Apply option ((Intrinsic Int))))))))
    |}]
;;

let%expect_test "list" =
  test_fragment
    {|
    type 'a list = | Nil | Cons of 'a * 'a list

    let x = Nil
    let y = Cons (3, Nil)
    let z = Cons (2, y)
    |};
  (* FIXME melse: should [x] have type [int list] now? *)
  [%expect
    {|
    (values (
      (x ((quantifiers (1)) (ty (Apply list ((Var 1))))))
      (y ((quantifiers ()) (ty (Apply list ((Intrinsic Int))))))
      (z ((quantifiers ()) (ty (Apply list ((Intrinsic Int))))))))
    |}]
;;

let%expect_test "list and options" =
  test_fragment
    {|
    type 'a list = | Nil | Cons of 'a * 'a list
    type 'a option = | None | Some of 'a

    let x = Cons (3, Nil)
    let hd =
      match x with
      | Nil -> None
      | Cons (x, _) -> Some x
    |};
  (* FIXME melse: [hd] should have type [int option] *)
  [%expect
    {|
    (values (
      (hd ((quantifiers (9)) (ty (Apply option ((Var 9))))))
      (x ((quantifiers ()) (ty (Apply list ((Intrinsic Int))))))))
    |}]
;;
