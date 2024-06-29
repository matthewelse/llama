open! Core
open! Import
open Helpers

let%expect_test "experiment" =
  Type.Id.For_testing.reset_counter ();
  Type.Var.For_testing.reset_counter ();
  let tn_bool = Located.dummy (Type_name.of_string "bool") in
  let ty_bool : Ast.Type.t = { desc = Apply (tn_bool, []); loc = Span.dummy } in
  let ast : Ast.t =
    [ Type_class_declaration
        { name = Located.dummy (Type_class_name.of_string "Eq")
        ; args = [ Located.dummy "'a" ]
        ; functions =
            [ { name = Located.dummy (Ident.of_string "eq")
              ; ty =
                  { desc =
                      Fun
                        ( [ { loc = Span.dummy; desc = Var "'a" }
                          ; { loc = Span.dummy; desc = Var "'a" }
                          ]
                        , ty_bool )
                  ; loc = Span.dummy
                  }
              }
            ]
        }
    ]
  in
  Pretty_print.pp_ast Format.std_formatter ast;
  [%expect {|
    class Eq ('a) : sig
      val eq : 'a -> 'a -> bool
    end
    |}];
  let tv = Type.Var.create () in
  Pretty_print.pp_polytype
    Format.std_formatter
    { ty =
        Fun ([ Var tv; Var tv ], Apply (Located.dummy @@ Type_name.of_string "bool", []))
    ; quantifiers = Type.Var.Set.singleton tv
    ; constraints =
        [ { type_class = Type_class_name.of_string "Eq"; args = [ Var tv ] }
        ; { type_class = Type_class_name.of_string "Ord"; args = [ Var tv ] }
        ]
    };
  [%expect {| 'a. Eq ('a), Ord ('a) => 'a -> 'a -> bool |}]
;;

let%expect_test "experiment" =
  test_fragment
    {|
  type 'a option = | None | Some of 'a

  intrinsic bool_equal : bool -> bool -> bool = "%int_equal"
  intrinsic bool_and : bool -> bool -> bool = "%int_equal"

  class Eq ('a) : sig
    val eq : 'a -> 'a -> bool
  end

  let f = fun(x, y) -> bool_and (eq(x, y), eq (Some x, Some y))
  |};
  [%expect
    {|
    (env (
      (values (
        (bool_and (
          (quantifiers ())
          (ty (
            Fun
            ((Apply ((value bool) (loc (<example>:5:23 <example>:5:27))) ())
             (Apply ((value bool) (loc (<example>:5:31 <example>:5:35))) ()))
            (Apply ((value bool) (loc (<example>:5:39 <example>:5:43))) ())))
          (constraints ())))
        (bool_equal (
          (quantifiers ())
          (ty (
            Fun
            ((Apply ((value bool) (loc (<example>:4:25 <example>:4:29))) ())
             (Apply ((value bool) (loc (<example>:4:33 <example>:4:37))) ()))
            (Apply ((value bool) (loc (<example>:4:41 <example>:4:45))) ())))
          (constraints ())))
        (eq (
          (quantifiers (2))
          (ty (
            Fun
            ((Var 2)
             (Var 2))
            (Apply ((value bool) (loc (<example>:8:25 <example>:8:29))) ())))
          (constraints (((type_class Eq) (args ((Var 2))))))))
        (f (
          (quantifiers (9))
          (ty (
            Fun
            ((Var 9)
             (Var 9))
            (Apply ((value bool) (loc (<example>:5:39 <example>:5:43))) ())))
          (constraints (
            ((type_class Eq)
             (args ((
               Apply
               ((value option) (loc (<example>:2:2 <example>:2:38)))
               ((Var 9))))))
            ((type_class Eq) (args ((Var 9))))))))))
      (type_declarations ((
        option (
          (shape (
            Variant
            (constructors (
              (((value None) (loc (<example>:2:21 <example>:2:25))) ())
              (((value Some) (loc (<example>:2:28 <example>:2:32))) ((Var 0)))))
            (id 0)))
          (args (0))
          (loc (<example>:2:2 <example>:2:38))))))
      (constructors (
        (None option)
        (Some option)))
      (fields       ())
      (type_classes ())))
    |}]
;;