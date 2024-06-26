open! Core
open! Import

let%expect_test "experiment" =
  Type.Id.For_testing.reset_counter ();
  Type.Var.For_testing.reset_counter ();
  let tn_int = Located.dummy (Type_name.of_string "int") in
  let _ty_int : Ast.Type.t = { desc = Apply (tn_int, []); loc = Span.dummy } in
  let ast : Ast.t =
    [ Type_declaration
        { name = tn_int
        ; type_params = []
        ; type_shape = Alias { desc = Intrinsic Int; loc = Span.dummy }
        ; loc = Span.dummy
        }
    ; Type_class_declaration
        { name = Located.dummy (Type_class_name.of_string "Eq")
        ; args = [ Located.dummy "'a" ]
        ; functions =
            [ { name = Located.dummy (Ident.of_string "eq")
              ; arg_types =
                  [ { loc = Span.dummy; desc = Var "'a" }
                  ; { loc = Span.dummy; desc = Var "'a" }
                  ]
              ; return_type =
                  { loc = Span.dummy
                  ; desc = Apply (Located.dummy (Type_name.of_string "bool"), [])
                  }
              }
            ]
        }
    ]
  in
  Pretty_print.pp_ast Format.std_formatter ast;
  [%expect
    {|
    type int = "%int"
    class Eq ('a) : sig
      val eq : 'a -> 'a -> bool
    end
    |}]
;;
