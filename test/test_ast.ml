open! Core
open! Llama

let%expect_test "experiment" =
  Type.Id.For_testing.reset_counter ();
  Type.Var.For_testing.reset_counter ();
  let ty_int = Type.Name.of_string "int" in
  let ast : Ast.t =
    [ Type_declaration
        { name = ty_int
        ; type_declaration =
            { type_params = []
            ; type_shape = Alias (Type.intrinsic Int)
            ; type_vars = Type.Var.Map.empty
            }
        }
    ; Intrinsic
        { name = Ident.of_string "add"
        ; intrinsic = Add_int
        ; type_ =
            { quantifiers = Type.Var.Set.empty
            ; ty = Fun (Type.const ty_int, Fun (Type.const ty_int, Type.const ty_int))
            }
        }
    ; Let
        { name = Ident.of_string "x"
        ; value =
            Apply
              ( Apply (Expression.var (Ident.of_string "add"), Expression.const_int 10)
              , Expression.const_int 50 )
        }
    ; Type_declaration
        { name = Type.Name.of_string "unit"
        ; type_declaration =
            { type_params = []
            ; type_shape =
                Variant { constructors = [ Constructor.of_string "Unit", None ] }
            ; type_vars = Type.Var.Map.empty
            }
        }
    ; (let tv = Type.Var.create () in
       Type_declaration
         { name = Type.Name.of_string "option"
         ; type_declaration =
             { type_params = [ tv ]
             ; type_shape =
                 Variant
                   { constructors =
                       [ Constructor.of_string "None", None
                       ; Constructor.of_string "Some", Some (Var tv)
                       ]
                   }
             ; type_vars = Type.Var.Map.of_alist_exn [ tv, "'a" ]
             }
         })
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
    type int = %int
    intrinsic add : int -> int -> int = %add_int
    let x = add(10)(50)
    type unit = | Unit
    type 'a option = | None | Some of 'a
    let x = None
    let x = Some (10)
    |}];
  let results = Infer.type_ast ast |> ok_exn in
  print_s
    [%message
      (results
       : Type.Poly.t Ident.Map.t
         * Type.Constructor.t Type.Name.Map.t
         * Type.Name.t Constructor.Map.t
         * Type.Name.t Field_name.Map.t)];
  [%expect {|
    (results
     (((add
        ((quantifiers ())
         (ty (Fun (Apply int ()) (Fun (Apply int ()) (Apply int ()))))))
       (x ((quantifiers ()) (ty (Apply option ((Intrinsic Int)))))))
      ((int ((shape (Alias (Intrinsic Int))) (args ())))
       (option
        ((shape (Variant (constructors ((None ()) (Some ((Var 0))))) (id 1)))
         (args (0))))
       (unit ((shape (Variant (constructors ((Unit ()))) (id 0))) (args ()))))
      ((None option) (Some option) (Unit unit)) ()))
    |}]
;;
