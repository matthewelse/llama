open! Core
open! Import

let%expect_test "experiment" =
  Type.Id.For_testing.reset_counter ();
  Type.Var.For_testing.reset_counter ();
  let tn_int = Located.dummy (Type_name.of_string "int") in
  let ty_int : Ast.Type.t = { desc = Apply (tn_int, []); loc = Span.dummy } in
  let ast : Ast.t =
    [ Intrinsic
        { name = Located.dummy (Ident.of_string "add")
        ; intrinsic = Located.dummy Intrinsic.Value.Add_int
        ; type_ =
            { quantifiers = []
            ; ty = { desc = Fun ([ ty_int; ty_int ], ty_int); loc = Span.dummy }
            }
        ; loc = Span.dummy
        }
    ; Let
        { name = Located.dummy (Ident.of_string "x")
        ; value =
            { desc =
                Apply
                  ( { desc = Var (Ident.of_string "add"); loc = Span.dummy }
                  , Located.dummy
                      [ Expression.const_int ~loc:Span.dummy 10
                      ; Expression.const_int ~loc:Span.dummy 50
                      ] )
            ; loc = Span.dummy
            }
        ; loc = Span.dummy
        }
    ; Type_declaration
        { name = Located.dummy (Type_name.of_string "unit")
        ; type_params = []
        ; type_shape =
            Variant
              { constructors = [ Located.dummy (Constructor.of_string "Unit"), None ] }
        ; loc = Span.dummy
        }
    ; Type_declaration
        { name = Located.dummy (Type_name.of_string "option")
        ; type_params = [ Located.dummy "'a" ]
        ; type_shape =
            Variant
              { constructors =
                  [ Located.dummy (Constructor.of_string "None"), None
                  ; ( Located.dummy (Constructor.of_string "Some")
                    , Some { desc = Var "'a"; loc = Span.dummy } )
                  ]
              }
        ; loc = Span.dummy
        }
    ; Let
        { name = Located.dummy (Ident.of_string "x")
        ; value =
            { desc = Construct (Located.dummy (Constructor.of_string "None"), None)
            ; loc = Span.dummy
            }
        ; loc = Span.dummy
        }
    ; Let
        { name = Located.dummy (Ident.of_string "x")
        ; value =
            { desc =
                Construct
                  ( Located.dummy (Constructor.of_string "Some")
                  , Some (Expression.const_int 10 ~loc:Span.dummy) )
            ; loc = Span.dummy
            }
        ; loc = Span.dummy
        }
    ]
  in
  Pretty_print.pp_ast Format.std_formatter ast;
  [%expect
    {|
    intrinsic add : int -> int -> int = "%add_int"
    let x = add(10, 50)
    type unit = | Unit
    type 'a option = | None | Some of 'a
    let x = None
    let x = Some (10)
    |}]
;;
