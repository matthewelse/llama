open! Core
open! Llama

let%expect_test "example" =
  Type.Var.For_testing.reset_counter ();
  let pp ty = Pretty_print.pp_type Format.std_formatter ty in
  let x = Type.Var.create () in
  let y = Type.Var.create () in
  let z = Type.Var.create () in
  let apply s l : Type.t = Apply (Type.Name.of_string s, l) in
  let def_type s args : Type.Name.t * Type.Constructor.t =
    Type.Name.of_string s, { args; shape = Alias (Type.intrinsic Int) }
  in
  let tyenv =
    List.fold
      [ def_type "a" []; def_type "j" [ x; y; z ]; def_type "f" [ x; y ] ]
      ~init:Type.Name.Map.empty
      ~f:(fun env (name, def) -> Map.set env ~key:name ~data:def)
  in
  let a = apply "a" [] in
  let j x y z = apply "j" [ x; y; z ] in
  let f x y = apply "f" [ x; y ] in
  let t1 = j (Var x) (Var y) (Var z) in
  pp t1;
  [%expect {| ('a, 'b, 'c) j |}];
  let t2 = j (f (Var y) (Var y)) (f (Var z) (Var z)) (f a a) in
  pp t2;
  [%expect {| (('a, 'a) f, ('b, 'b) f, (a, a) f) j |}];
  let u = Type.unify t1 t2 ~tyenv in
  print_s [%sexp (u : Type.t Type.Var.Map.t)];
  [%expect
    {|
    ((0 (Apply f ((Var 1) (Var 1)))) (1 (Apply f ((Var 2) (Var 2))))
     (2 (Apply f ((Apply a ()) (Apply a ())))))
    |}];
  let t3 = Type.subst t1 ~replacements:u in
  pp t3;
  [%expect {| (('a, 'a) f, ('b, 'b) f, (a, a) f) j |}]
;;
