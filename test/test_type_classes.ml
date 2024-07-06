open! Core
open! Import
open Helpers

let%expect_test "experiment" =
  test_fragment
    ~output:`Values
    {|
  type 'a option = | None | Some of 'a

  intrinsic bool_equal : (bool, bool) -> bool = "%int_equal"
  intrinsic bool_and : (bool, bool) -> bool = "%int_land"
  intrinsic int_add : (int, int) -> int = "%int_add"
  intrinsic int_equal : (int, int) -> bool = "%int_equal"
  intrinsic neg : int -> int = "%int_neg"

  class Eq ('a) : sig
    val eq : ('a, 'a) -> bool
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
    val compare : ('a, 'a) -> int
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
