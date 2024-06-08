open! Core
open! Llama

let%expect_test "examples" =
  let open Expression in
  let type_of expr =
    let typ =
      Infer.type_of_let_binding
        expr
        ~env:Ident.Map.empty
        ~tyenv:Type.Name.Map.empty
        ~constructors:Constructor.Map.empty
        ~fields:Field_name.Map.empty
      |> ok_exn
    in
    Pretty_print.pp_polytype Format.std_formatter typ
  in
  let zero = const_int 0 in
  type_of zero;
  [%expect {| %int |}];
  let ident = Ident.of_string in
  let x = ident "x" in
  let y = ident "y" in
  let id : Expression.t = lambda x (var x) in
  type_of id;
  [%expect {| 'a. 'a -> 'a |}];
  let first : Expression.t = lambda x (lambda y (var x)) in
  type_of first;
  [%expect {| 'a 'b. 'a -> 'b -> 'a |}];
  let swap : Expression.t =
    Lambda (ident "x", Lambda (ident "y", Tuple [ Var (ident "x"); Var (ident "y") ]))
  in
  type_of swap;
  [%expect {| 'a 'b. 'a -> 'b -> ('b * 'a) |}];
  let we_love_polymorphism : Expression.t =
    let_
      ~name:(ident "id")
      ~value:id
      ~in_:
        (let_
           ~name:(ident "_")
           ~value:(Apply (Var (ident "id"), zero))
           ~in_:(Apply (Var (ident "id"), Const (String "hello, world"))))
  in
  type_of we_love_polymorphism;
  [%expect {| %string |}]
;;
