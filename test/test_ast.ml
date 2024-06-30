open! Core
open! Import

let%expect_test "experiment" =
  Type.Id.For_testing.reset_counter ();
  Type.Var.For_testing.reset_counter ();
  let tn_int = Type_name.of_string "int", Span.dummy in
  let ty_int : Ast.Type.t = Apply ((tn_int, []), Span.dummy) in
  let ast : Ast.t =
    [ Intrinsic
        { name = Ident.of_string "add", Span.dummy
        ; intrinsic = Intrinsic.Value.Int_add, Span.dummy
        ; type_ =
            { quantifiers = []
            ; body = Fun (([ ty_int; ty_int ], ty_int), Span.dummy)
            ; constraints = []
            }
        ; loc = Span.dummy
        }
    ; Let
        { name = Ident.of_string "x", Span.dummy
        ; value =
            Apply
              ( ( Var (Ident.of_string "add", Span.dummy)
                , [ Expression.const_int ~annot:Span.dummy 10
                  ; Expression.const_int ~annot:Span.dummy 50
                  ] )
              , Span.dummy )
        ; loc = Span.dummy
        }
    ; Type_declaration
        { name = Type_name.of_string "unit", Span.dummy
        ; type_params = []
        ; type_shape =
            Variant
              { constructors = [ (Constructor.of_string "Unit", Span.dummy), None ] }
        ; loc = Span.dummy
        }
    ; Type_declaration
        { name = Type_name.of_string "option", Span.dummy
        ; type_params = [ "'a", Span.dummy ]
        ; type_shape =
            Variant
              { constructors =
                  [ (Constructor.of_string "None", Span.dummy), None
                  ; ( (Constructor.of_string "Some", Span.dummy)
                    , Some (Var ("'a", Span.dummy)) )
                  ]
              }
        ; loc = Span.dummy
        }
    ; Let
        { name = Ident.of_string "x", Span.dummy
        ; value =
            Construct (((Constructor.of_string "None", Span.dummy), None), Span.dummy)
        ; loc = Span.dummy
        }
    ; Let
        { name = Ident.of_string "x", Span.dummy
        ; value =
            Construct
              ( ( (Constructor.of_string "Some", Span.dummy)
                , Some (Expression.const_int 10 ~annot:Span.dummy) )
              , Span.dummy )
        ; loc = Span.dummy
        }
    ]
  in
  Pretty_print.pp_ast Format.std_formatter ast;
  [%expect
    {|
    intrinsic add : int -> int -> int = "%int_add"
    let x = add(10, 50)
    type unit = | Unit
    type 'a option = | None | Some of 'a
    let x = None
    let x = Some (10)
    |}]
;;
