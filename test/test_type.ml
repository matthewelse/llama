open! Core
open! Import

let%test_module "free_type_vars" =
  (module struct
    let var () = Type.Var.create ()

    let ftvs typ =
      Type.Var.For_testing.reset_counter ();
      Type.Id.For_testing.reset_counter ();
      let ftvs = Type.free_type_vars typ in
      print_s [%message (ftvs : Type.Var.Set.t)]
    ;;

    let%expect_test "var" =
      ftvs (Var (var (), ()));
      [%expect {| (ftvs (0)) |}]
    ;;

    let%expect_test "apply" =
      ftvs (Apply (((Type_name.of_string "x", Span.dummy), [ Var (var (), ()) ]), ()));
      [%expect {| (ftvs (0)) |}]
    ;;

    let%expect_test "fun" =
      ftvs (Fun (([ Var (var (), ()) ], Var (var (), ())), ()));
      [%expect {| (ftvs (0 1)) |}]
    ;;

    let%expect_test "tuple" =
      ftvs (Tuple ([ Var (var (), ()); Var (var (), ()) ], ()));
      [%expect {| (ftvs (0 1)) |}]
    ;;

    let%expect_test "intrinsic" =
      ftvs (Type.intrinsic Int);
      [%expect {| (ftvs ()) |}]
    ;;
  end)
;;

let%test_module "occurs" =
  (module struct
    let var () = Type.Var.create ()

    let occurs typ var =
      Type.Var.For_testing.reset_counter ();
      Type.Id.For_testing.reset_counter ();
      let occurs = Type.occurs typ ~var in
      print_s [%message (occurs : bool)]
    ;;

    let%expect_test "var" =
      occurs (Var (var (), ())) (var ());
      [%expect {| (occurs false) |}]
    ;;

    let%expect_test "occurs" =
      occurs (Var (var (), ())) (var ());
      [%expect {| (occurs false) |}]
    ;;
  end)
;;

let%expect_test "example" =
  Type.Var.For_testing.reset_counter ();
  Type.Id.For_testing.reset_counter ();
  let pp ty = Pretty_print.pp_type Format.std_formatter ty in
  let x = Type.Var.create () in
  let y = Type.Var.create () in
  let z = Type.Var.create () in
  let apply s l : Type.t = Apply (((Type_name.of_string s, Span.dummy), l), ()) in
  let a = apply "a" [] in
  let j x y z = apply "j" [ x; y; z ] in
  let f x y = apply "f" [ x; y ] in
  let t1 = j (Type.var x) (Type.var y) (Type.var z) in
  pp t1;
  [%expect {| ('a, 'b, 'c) j |}];
  let t2 = j (f (Type.var y) (Type.var y)) (f (Type.var z) (Type.var z)) (f a a) in
  pp t2;
  [%expect {| (('a, 'a) f, ('b, 'b) f, (a, a) f) j |}]
;;
