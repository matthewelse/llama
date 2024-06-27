open! Core
open! Import

let%expect_test "experiment" =
  Type.Id.For_testing.reset_counter ();
  Type.Var.For_testing.reset_counter ();
  let tn_bool = Located.dummy (Type_name.of_string "bool") in
  let ty_bool : Ast.Type.t = { desc = Apply (tn_bool, []); loc = Span.dummy } in
  let ast : Ast.t =
    [ Type_declaration
        { name = tn_bool
        ; type_params = []
        ; type_shape = Alias { desc = Intrinsic Bool; loc = Span.dummy }
        ; loc = Span.dummy
        }
    ; Type_class_declaration
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
  [%expect
    {|
    type bool = "%bool"
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
    ; constraints = [ { type_class = Type_class_name.of_string "Eq"; args = [ tv ] } ]
    };
  [%expect {| 'a. Eq ('a) => 'a -> 'a -> bool |}]
;;
