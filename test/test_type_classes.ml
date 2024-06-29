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
        ; arg = Located.dummy "'a"
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
        ; constraints = []
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
        [ { type_class = Type_class_name.of_string "Eq"; arg = Var tv }
        ; { type_class = Type_class_name.of_string "Ord"; arg = Var tv }
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

  impl Eq (bool) = struct
    let eq = fun(x, y) -> bool_equal(x, y)
  end

  class Ord ('a) where Eq ('a) : sig
    val compare : 'a -> 'a -> int
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
        (compare (
          (quantifiers (6))
          (ty (
            Fun
            ((Var 6)
             (Var 6))
            (Apply ((value int) (loc (<example>:16:30 <example>:16:33))) ())))
          (constraints (((type_class Ord) (arg (Var 6)))))))
        (eq (
          (quantifiers (3))
          (ty (
            Fun
            ((Var 3)
             (Var 3))
            (Apply ((value bool) (loc (<example>:8:25 <example>:8:29))) ())))
          (constraints (((type_class Eq) (arg (Var 3)))))))
        (f (
          (quantifiers (13))
          (ty (
            Fun
            ((Var 13)
             (Var 13))
            (Apply ((value bool) (loc (<example>:5:39 <example>:5:43))) ())))
          (constraints (
            ((type_class Eq)
             (arg (
               Apply
               ((value option) (loc (<example>:2:2 <example>:2:38)))
               ((Var 13)))))
            ((type_class Eq) (arg (Var 13)))))))))
      (type_declarations (
        (bool ((shape (Intrinsic Bool)) (args ()) (loc (:0:-1 :0:-1))))
        (int ((shape (Intrinsic Int)) (args ()) (loc (:0:-1 :0:-1))))
        (option (
          (shape (
            Variant
            (constructors (
              (((value None) (loc (<example>:2:21 <example>:2:25))) ())
              (((value Some) (loc (<example>:2:28 <example>:2:32))) ((Var 1)))))
            (id 0)))
          (args (1))
          (loc (<example>:2:2 <example>:2:38))))
        (ref ((shape (Intrinsic Ref)) (args (0)) (loc (:0:-1 :0:-1))))
        (string ((shape (Intrinsic String)) (args ()) (loc (:0:-1 :0:-1))))))
      (constructors (
        (None option)
        (Some option)))
      (fields       ())
      (type_classes ())
      (type_class_implementations ((
        (constraints ())
        (type_class Eq)
        (for_type (
          Apply ((value bool) (loc (<example>:11:11 <example>:11:15))) ())))))))
    |}]
;;
