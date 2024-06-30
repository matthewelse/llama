open! Core
open! Import
open Helpers

let%expect_test "experiment" =
  Type.Id.For_testing.reset_counter ();
  Type.Var.For_testing.reset_counter ();
  let tn_bool = Type_name.of_string "bool", Span.dummy in
  let ty_bool : Ast.Type.t = Apply ((tn_bool, []), Span.dummy) in
  let ast : Ast.t =
    [ Type_class_declaration
        { name = Type_class_name.of_string "Eq", Span.dummy
        ; arg = "'a", Span.dummy
        ; functions =
            [ { name = Ident.of_string "eq", Span.dummy
              ; ty =
                  Fun
                    ( ([ Var ("'a", Span.dummy); Var ("'a", Span.dummy) ], ty_bool)
                    , Span.dummy )
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
    { body =
        Fun
          ( ( [ Type.var tv; Type.var tv ]
            , Apply (((Type_name.of_string "bool", Span.dummy), []), ()) )
          , () )
    ; quantifiers = [ tv ]
    ; constraints =
        [ { type_class = Type_class_name.of_string "Eq"; arg = Var (tv, ()) }
        ; { type_class = Type_class_name.of_string "Ord"; arg = Var (tv, ()) }
        ]
    };
  [%expect {| 'a. Eq ('a), Ord ('a) => 'a -> 'a -> bool |}]
;;

let%expect_test "experiment" =
  test_fragment
    ~output:`Values
    {|
  type 'a option = | None | Some of 'a

  intrinsic bool_equal : bool -> bool -> bool = "%int_equal"
  intrinsic bool_and : bool -> bool -> bool = "%int_land"
  intrinsic int_add : int -> int -> int = "%int_add"
  intrinsic int_equal : int -> int -> bool = "%int_equal"
  intrinsic neg : int -> int = "%int_neg"

  class Eq ('a) : sig
    val eq : 'a -> 'a -> bool
  end

  impl Eq (bool) = struct
    let eq = fun(x, y) -> bool_equal(x, y)
  end

  impl Eq (int) = struct
    let eq = fun(x, y) -> int_equal(x, y)
  end

  impl Eq ('a option) where Eq ('a) = struct
    let eq = fun(x, y) ->
      match x with
      | None -> (match y with None -> true | Some _ -> false)
      | Some x -> (match y with None -> false | Some y -> eq(x, y))
  end

  class Ord ('a) where Eq ('a) : sig
    val compare : 'a -> 'a -> int
  end

  impl Ord ('a option) where Ord ('a) = struct
    let compare = fun (x, y) ->
      match x with
      | None -> (match y with None -> 0 | Some _ -> neg(1))
      | Some x -> (match y with None -> 1 | Some y -> compare(x, y))
  end

  let f = fun(x, y) -> bool_and (eq(x, y), eq (Some x, Some y))

  let f2 = fun(x, y) -> int_add (compare (x, y), compare (Some x, Some y))

  let x = f(1, 2)
  let y = f2 (1, 2)
  |};
  [%expect
    {|
    val bool_and : bool -> bool -> bool
    val bool_equal : bool -> bool -> bool
    val compare : 'a. Ord ('a) => 'a -> 'a -> int
    val eq : 'a. Eq ('a) => 'a -> 'a -> bool
    val f : 'a. Eq (('a) option), Eq ('a) => 'a -> 'a -> bool
    val f2 : 'a. Ord (('a) option), Ord ('a) => 'a -> 'a -> int
    val int_add : int -> int -> int
    val int_equal : int -> int -> bool
    val neg : int -> int
    val x : Eq (int), Eq ((int) option) => bool
    val y : Ord (int), Ord ((int) option) => int
    |}]
;;
