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
      ftvs
        (Apply
           (((Type_name.of_string "x", `Position Span.dummy), [ Var (var (), ()) ]), ()));
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
