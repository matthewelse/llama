open! Core
open! Import

let%expect_test "experiment" =
  Type.Id.For_testing.reset_counter ();
  Type.Var.For_testing.reset_counter ();
  let ty_int = Type_name.of_string "int" in
  let ast : Ast.t =
    [ Type_declaration
        { name = ty_int; type_params = []; type_shape = Alias (Intrinsic Int) }
    ; Intrinsic
        { name = Ident.of_string "add"
        ; intrinsic = Add_int
        ; type_ =
            { quantifiers = []
            ; ty =
                Fun
                  ([ Ast.Type.const ty_int; Ast.Type.const ty_int ], Ast.Type.const ty_int)
            }
        }
    ; Let
        { name = Ident.of_string "x"
        ; value =
            Apply
              ( Var (Ident.of_string "add")
              , [ Expression.const_int 10; Expression.const_int 50 ] )
        }
    ; Type_declaration
        { name = Type_name.of_string "unit"
        ; type_params = []
        ; type_shape = Variant { constructors = [ Constructor.of_string "Unit", None ] }
        }
    ; Type_declaration
        { name = Type_name.of_string "option"
        ; type_params = [ "'a" ]
        ; type_shape =
            Variant
              { constructors =
                  [ Constructor.of_string "None", None
                  ; Constructor.of_string "Some", Some (Var "'a")
                  ]
              }
        }
    ; Let
        { name = Ident.of_string "x"
        ; value = Construct (Constructor.of_string "None", None)
        }
    ; Let
        { name = Ident.of_string "x"
        ; value = Construct (Constructor.of_string "Some", Some (Expression.const_int 10))
        }
    ]
  in
  Pretty_print.pp_ast Format.std_formatter ast;
  [%expect
    {|
    type int = "%int"
    intrinsic add : int -> int -> int = "%add_int"
    let x = add(10, 50)
    type unit = | Unit
    type 'a option = | None | Some of 'a
    let x = None
    let x = Some (10)
    |}];
  let results = Infer.type_ast ast |> ok_exn in
  print_s [%message (results : Infer.Env.t)];
  [%expect
    {|
    (results (
      (values (
        (add (
          (quantifiers ())
          (ty (
            Fun
            ((Apply int ())
             (Apply int ()))
            (Apply int ())))))
        (x ((quantifiers ()) (ty (Apply option ((Intrinsic Int))))))))
      (type_declarations (
        (int ((shape (Alias (Intrinsic Int))) (args ())))
        (option (
          (shape (Variant (constructors ((None ()) (Some ((Var 1))))) (id 1)))
          (args (1))))
        (unit ((shape (Variant (constructors ((Unit ()))) (id 0))) (args ())))))
      (constructors (
        (None option)
        (Some option)
        (Unit unit)))
      (fields ())))
    |}]
;;
