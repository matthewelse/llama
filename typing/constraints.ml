open! Core
open! Import
module Constraint = Solver.Constraint

let rec check_pattern
  (pattern : Ast.Pattern.t)
  expected_ty
  ~(constraints : Constraint.t Queue.t)
  ~env
  : Typed_ast.Pattern.t * Env.t
  =
  match pattern with
  | Var (name, _) ->
    let env = Env.with_var env name (Type.Poly.mono expected_ty) in
    Var (name, ()), env
  | Construct (((constructor_name, constructor_loc), arg), _) ->
    (* FIXME: share this code with [check_constructor] *)
    let type_name = Env.constructor_exn env constructor_name ~loc:constructor_loc in
    let%tydi { args = type_args; shape; loc = _ } =
      Env.type_declaration_exn env type_name ~loc:constructor_loc
    in
    (* Make fresh type variables for the args *)
    let type_args =
      List.map type_args ~f:(fun old -> old, Type.var (Type_var.create ()))
    in
    let type_arg_mapping = Type_var.Map.of_alist_exn type_args in
    let type_args = List.map type_args ~f:snd in
    let constructors =
      match shape with
      | Variant { constructors; id = _ } -> constructors
      | _ ->
        (* It's a bug for this type not to be a variant. We don't need to worry about aliases, since
           [Env.constructor] always points at the original declaration. *)
        failwith "Internal Compiler Error"
    in
    let arg_type =
      (* It's a bug for this constructor not to exist in the variant. *)
      List.Assoc.find_exn
        constructors
        (constructor_name, constructor_loc)
        ~equal:[%equal: Constructor.t * _]
    in
    (match arg_type, arg with
     | None, None ->
       Queue.enqueue
         constraints
         (Same_type
            ( expected_ty
            , Apply (((type_name, `Position constructor_loc), type_args), ())
            , [ Pattern_should_have_type (pattern, expected_ty) ] ));
       Construct (((constructor_name, ()), None), ()), env
     | Some arg_type, Some arg_pattern ->
       let arg_type = Type.subst arg_type ~replacements:type_arg_mapping in
       let typed_ast, env = check_pattern arg_pattern arg_type ~constraints ~env in
       Queue.enqueue
         constraints
         (Same_type
            ( expected_ty
            , Apply (((type_name, `Position constructor_loc), type_args), ())
            , [ Pattern_should_have_type (pattern, expected_ty) ] ));
       Construct (((constructor_name, ()), Some typed_ast), ()), env
     | Some _, None ->
       (* FIXME: this should produce an error that points to the (argument-less) pattern, with an info
          annotation pointing to the original type declaration. *)
       Reporter.fatal
         ~loc:(Asai.Range.of_lex_range constructor_loc)
         Type_error
         [%string
           "Constructor %{constructor_name#Constructor} expects an argument, but none \
            was provided."]
     | None, Some _ ->
       (* FIXME: this should produce an error that points to the pattern arguments, with an info
          annotation pointing to the original type declaration. *)
       Reporter.fatal
         ~loc:(Asai.Range.of_lex_range constructor_loc)
         Type_error
         [%string
           "Constructor %{constructor_name#Constructor} does not expect an argument, but \
            one was provided."])
  | Tuple (patterns, _loc) ->
    let type_vars = List.map patterns ~f:(fun pattern -> pattern, Type_var.create ()) in
    let env, typed_asts =
      List.fold_map type_vars ~init:env ~f:(fun env (pattern, type_var) ->
        let typed_ast, env =
          check_pattern pattern (Var (type_var, ())) ~constraints ~env
        in
        env, typed_ast)
    in
    Queue.enqueue
      constraints
      (Same_type
         ( expected_ty
         , Tuple
             (List.map type_vars ~f:(fun (_, type_var) : Type.t -> Var (type_var, ())), ())
         , [ Pattern_should_have_type (pattern, expected_ty) ] ));
    Tuple (typed_asts, ()), env
;;

let rec infer' (expr : Expression.t) ~(constraints : Constraint.t Queue.t) ~env
  : Typed_ast.Expression.t * Type.t
  =
  match expr with
  | Const (Int value, _) -> Const (Int value, ()), Type.intrinsic Int
  | Const (String value, _) -> Const (String value, ()), Type.intrinsic String
  | Var (v, loc) ->
    let poly_type = Env.value_exn env v ~loc in
    let type_, type_constraints = Type.Poly.init poly_type in
    List.iter type_constraints ~f:(fun { type_class; arg } ->
      Queue.enqueue
        constraints
        (Implements_type_class
           (type_class, arg, [ Var_requires_type_class ({ value = v; loc }, type_class) ])));
    Var (v, ()), type_
  | Lambda ((args, body), _) ->
    let arg_types = List.map args ~f:(fun name -> fst name, Type_var.create ()) in
    let env = Env.with_vars env arg_types in
    let body_typed_ast, body_type = infer' body ~constraints ~env in
    ( Lambda
        ( (List.map arg_types ~f:(fun (name, var) -> name, Type.var var), body_typed_ast)
        , () )
    , Type.Fun ((List.map arg_types ~f:(fun (_, var) -> Type.var var), body_type), ()) )
  | Apply ((function_, arg_values), _) ->
    (* type_of([$function_($arg_values...)])

       - [arg_values] is a list of expressions, so just infer the type of each expression.
       - Infer the type of [function_].
       - [check] that the inferred type of [function_] is [Fun _].

       We do this in two phases: first we generate fresh type variables for each argument to the
       function, and then we add constraints that check that each type variable is equal to the
       inferred type of each value in [arg_values]. This pushes type errors "down" into each
       argument, rather than checking the function as a whole, giving better error messages (at
       the cost of more constraints to iterate through). *)
    let fresh_arg_tys = List.map arg_values ~f:(fun _ -> Type.var (Type_var.create ())) in
    let return_ty = Type.var (Type_var.create ()) in
    let fun_ty : Type.t = Fun ((fresh_arg_tys, return_ty), ()) in
    let fun_ast = check function_ fun_ty ~constraints ~env in
    let arg_asts =
      List.map2_exn arg_values fresh_arg_tys ~f:(fun arg_value fresh_ty ->
        let arg_ast, arg_ty = infer' arg_value ~constraints ~env in
        Queue.enqueue
          constraints
          (Same_type
             ( arg_ty
             , fresh_ty
             , [ Expression_should_have_type (arg_value, fresh_ty)
               ; Expression_should_have_type (function_, fun_ty)
               ] ));
        arg_ast)
    in
    Apply ((fun_ast, arg_asts), ()), return_ty
  | Let { name; value; in_; annotation = loc } ->
    (* [let v = x in y] should be equivalent to [(fun v -> y) x] *)
    infer' (Apply ((Lambda (([ name ], in_), loc), [ value ]), loc)) ~constraints ~env
  | Tuple (elems, _) ->
    let types = List.map elems ~f:(infer' ~constraints ~env) in
    let asts, types = List.unzip types in
    Tuple (asts, ()), Tuple (types, ())
  | Construct ((constructor_name, arg), _) ->
    infer_constructor constructor_name arg ~constraints ~env
  | Record (fields, loc) -> infer_record fields ~env ~constraints ~loc
  | Match { scrutinee; cases; annotation = loc } ->
    (* FIXME: check match completeness *)
    let scrutinee_typed_ast, scrutinee_ty = infer' scrutinee ~constraints ~env in
    let%tydi body_ty, typed_cases =
      List.fold_map cases ~init:None ~f:(fun expected_body_type (pattern, body) ->
        let pattern_ast, env = check_pattern pattern scrutinee_ty ~constraints ~env in
        let body_ast, body_ty =
          match expected_body_type with
          | None -> infer' body ~constraints ~env
          | Some expected_type ->
            let body_ast = check body expected_type ~constraints ~env in
            body_ast, expected_type
        in
        Some body_ty, (pattern_ast, body_ast))
    in
    let body_ty =
      (* The body type should be defined, since [cases] should never be empty. *)
      match body_ty with
      | None ->
        Reporter.fatal
          ~loc:(Asai.Range.of_lex_range loc)
          Type_error
          "Internal compiler error: body type undefined (empty match expression?)"
      | Some body_ty -> body_ty
    in
    ( Match { scrutinee = scrutinee_typed_ast; cases = typed_cases; annotation = () }
    , body_ty )
  | TFun _ | TApply _ ->
    (* Not used in the untyped AST. *)
    .

and infer_record fields ~env ~constraints ~loc =
  let type_name =
    (* Arbitrarily choose the first field as the "representative" to pick a type for this record. *)
    Env.field_exn env (List.hd_exn fields |> fst |> fst) ~loc
  in
  let%tydi { args = type_args; shape; loc = _ } =
    Env.type_declaration_exn env type_name ~loc
  in
  (* Make fresh type variables for the args *)
  let type_args = List.map type_args ~f:(fun old -> old, Type.var (Type_var.create ())) in
  let type_arg_mapping = Type_var.Map.of_alist_exn type_args in
  let type_args = List.map type_args ~f:snd in
  let field_types =
    match shape with
    | Record { fields = field_types; id = _ } -> field_types
    | _ ->
      (* It's a bug for this type not to be a variant. We don't need to worry about aliases, since
         ; loc  = _    [Env.constructor] always points at the original declaration. *)
      failwith "Internal Compiler Error"
  in
  let fields = List.sort fields ~compare:[%compare: (Field_name.t * _) * _] in
  let field_types = List.sort field_types ~compare:[%compare: (Field_name.t * _) * _] in
  let result =
    match
      List.map2
        fields
        field_types
        ~f:
          (fun
            ((field_name, field_name_loc), field_value)
            ((expected_field_name, _), expected_field_type)
          ->
          let `field_names_match =
            if Field_name.equal field_name expected_field_name
            then `field_names_match
            else
              (* FIXME: Better error message could include "did you mean", error should point at the field
                 name in the expression, possibly with an "info" diagnostic pointing at the original
                 definition.

                 Error message could also include a reference to why we thought this record had this type
                 (i.e. because of the arbitrarily-chosen field [List.hd_exn fields]). *)
              Reporter.fatal
                ~loc:(Asai.Range.of_lex_range field_name_loc)
                Type_error
                [%string
                  "Record field name mismatch: %{field_name#Field_name} does not match \
                   expected name %{expected_field_name#Field_name}"]
          in
          let expected_field_type =
            Type.subst expected_field_type ~replacements:type_arg_mapping
          in
          field_name, check field_value expected_field_type ~constraints ~env)
    with
    | Unequal_lengths ->
      let expected_fields =
        List.map field_types ~f:(fun ((value, _), _) -> value) |> Field_name.Set.of_list
      in
      let specified_fields =
        List.map fields ~f:(fun ((value, _), _) -> value) |> Field_name.Set.of_list
      in
      let missing_fields = Set.diff expected_fields specified_fields in
      let missing =
        if Set.is_empty missing_fields
        then None
        else (
          let field_or_fields, were_or_was =
            if Set.length missing_fields = 1 then "field", "was" else "fields", "were"
          in
          let fields =
            Set.to_list missing_fields
            |> List.map ~f:Field_name.to_string
            |> String.concat ~sep:", "
          in
          Some [%string "%{field_or_fields} %{fields} %{were_or_was} missing."])
      in
      let unnecessary_fields = Set.diff specified_fields expected_fields in
      let unnecessary =
        if Set.is_empty unnecessary_fields
        then None
        else (
          let field_or_fields, were_or_was =
            if Set.length unnecessary_fields = 1 then "field", "was" else "fields", "were"
          in
          let fields =
            Set.to_list unnecessary_fields
            |> List.map ~f:Field_name.to_string
            |> String.concat ~sep:", "
          in
          Some [%string "%{field_or_fields} %{fields} %{were_or_was} not expected."])
      in
      let error_message =
        List.filter_opt
          [ Some "Record value did not have the correct number of arguments."
          ; missing
          ; unnecessary
          ]
        |> String.concat ~sep:" "
      in
      Reporter.fatal ~loc:(Asai.Range.of_lex_range loc) Type_error error_message
    | Ok result -> result
  in
  let typed_fields =
    List.map result ~f:(fun (name, typed_ast) -> (name, ()), typed_ast)
  in
  Record (typed_fields, ()), Type.Apply (((type_name, `Position loc), type_args), ())

and infer_constructor (constructor_name, constructor_name_loc) arg ~constraints ~env =
  let type_name = Env.constructor_exn env constructor_name ~loc:constructor_name_loc in
  let%tydi { args = type_args; shape; loc = type_decl_loc } =
    Env.type_declaration_exn env type_name ~loc:constructor_name_loc
  in
  (* Make fresh type variables for the args *)
  let type_args = List.map type_args ~f:(fun old -> old, Type.var (Type_var.create ())) in
  let type_arg_mapping = Type_var.Map.of_alist_exn type_args in
  let type_args = List.map type_args ~f:snd in
  let constructors =
    match shape with
    | Variant { constructors; id = _ } -> constructors
    | _ ->
      (* It's a bug for this type not to be a variant. We don't need to worry about aliases, since
         [Env.constructor] always points at the original declaration. *)
      failwith "Internal Compiler Error"
  in
  let constructor =
    (* It's a bug for this constructor not to exist in the variant. *)
    List.Assoc.find_exn
      constructors
      (constructor_name, constructor_name_loc)
      ~equal:[%equal: Constructor.t * _]
  in
  let out : Typed_ast.Expression.t =
    match constructor, arg with
    | None, None ->
      (* No constraints *)
      Construct (((constructor_name, ()), None), ())
    | Some constructor_typ, Some value ->
      let constructor_typ = Type.subst constructor_typ ~replacements:type_arg_mapping in
      let arg_typed_ast = check value constructor_typ ~constraints ~env in
      Construct (((constructor_name, ()), Some arg_typed_ast), ())
    | None, Some arg ->
      (* FIXME: this should point at the location of [value]. *)
      Reporter.fatal
        ~loc:(Asai.Range.of_lex_range (Expression.loc arg))
        Type_error
        [%string
          "Arguments were provided to constructor %{constructor_name#Constructor}, but \
           none were expected."]
    | Some _, None ->
      (* FIXME: better error message would include the expected type. We'll need to move more of the
         pretty printing logic into this library.

         This should point at the location of the constructor. Ideally the diagnostic should also
         refer to the constructor in the original type declaration too. *)
      Reporter.fatal
        ~loc:(Asai.Range.of_lex_range constructor_name_loc)
        Type_error
        [%string
          "Arguments of were expected for constructor %{constructor_name#Constructor}, \
           but none were provided."]
  in
  out, Type.Apply (((type_name, type_decl_loc), type_args), ())

and check (expr : Expression.t) (expected_ty : Type.t) ~constraints ~env
  : Typed_ast.Expression.t
  =
  let typed_ast, ty = infer' expr ~constraints ~env in
  Queue.enqueue
    constraints
    (Same_type (ty, expected_ty, [ Expression_should_have_type (expr, expected_ty) ]));
  typed_ast
;;

let infer expr ~env =
  let constraints = Queue.create () in
  let typed_ast, type_ = infer' expr ~constraints ~env in
  Queue.to_list constraints, typed_ast, type_
;;

module For_testing = struct
  let check_pattern = check_pattern
end
