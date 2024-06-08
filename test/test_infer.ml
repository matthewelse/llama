open! Core
open! Llama

let%expect_test "examples" =
  let type_of expr =
    let typ = Expression.type_of_let_binding expr |> ok_exn in
    Pretty_print.pp_polytype Format.std_formatter typ
  in
  let zero = Expression.Const (Int 0) in
  type_of zero;
  [%expect {| %int |}];
  let ident = Ident.of_string in
  let id : Expression.t = Lambda (ident "x", Var (ident "x")) in
  type_of id;
  [%expect {| 'a. 'a -> 'a |}];
  let swap : Expression.t = Lambda (ident "x", Lambda (ident "y", Var (ident "x"))) in
  type_of swap;
  [%expect {| 'a 'b. 'a -> 'b -> 'a |}];
  let swap : Expression.t =
    Lambda (ident "x", Lambda (ident "y", Tuple [ Var (ident "x"); Var (ident "y") ]))
  in
  type_of swap;
  [%expect {| 'a 'b. 'a -> 'b -> ('b * 'a) |}];
  let we_love_polymorphism : Expression.t =
    Let
      { name = ident "id"
      ; value = id
      ; body =
          Let
            { name = ident "_"
            ; value = Apply (Var (ident "id"), zero)
            ; body = Apply (Var (ident "id"), Const (String "hello, world"))
            }
      }
  in
  type_of we_love_polymorphism;
  [%expect {| %string |}]
;;
