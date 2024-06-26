open! Core
open! Import

module Annotation = struct
  type t =
    | Expression_should_have_type of Expression.t * Type.t
    | Pattern_should_have_type of Pattern.t * Type.t
  [@@deriving sexp_of]
end

module Constraint = struct
  type 'a t = Same_type of Type.t * Type.t * 'a [@@deriving sexp_of]
end

module Annotations = struct
  (* FIXME melse: factor out [Nonempty_list.t] *)
  type t = ( :: ) of Annotation.t * Annotation.t list [@@deriving sexp_of]
end

type t = Annotations.t Constraint.t list [@@deriving sexp_of]

let add (t : t) c = c :: t
let singleton c : t = [ c ]
let empty = []
let merge t1 t2 = t1 @ t2
let merge_list ts = List.fold ts ~init:empty ~f:merge
let to_list t = t

let rec infer (expr : Expression.t) ~env =
  let open Result.Let_syntax in
  let no_constraints ty : Type.t * t = ty, empty in
  match expr.desc with
  | Const (Int _) -> Ok (no_constraints (Intrinsic Int))
  | Const (String _) -> Ok (no_constraints (Intrinsic String))
  | Var v ->
    let%bind poly_type = Env.value env v ~loc:expr.loc in
    let type_ = Type.Poly.init poly_type in
    Ok (no_constraints type_)
  | Lambda (args, body) ->
    let arg_types = List.map args ~f:(fun name -> name, Type.Var.create ()) in
    let env = Env.with_vars env arg_types in
    let%bind body_type, body_constraints = infer body ~env in
    Ok
      ( Type.Fun (List.map arg_types ~f:(fun (_, var) -> Type.var var), body_type)
      , body_constraints )
  | Apply (function_, arg_values) ->
    (* type_of([$function_($arg_values...)])

       - [arg_values] is a list of expressions, so just infer the type of each expression.
       - Infer the type of [function_].
       - [check] that the inferred type of [function_] is [Fun _].

       We do this in two phases: first we generate fresh type variables for each argument to the
       function, and then we add constraints that check that each type variable is equal to the
       inferred type of each value in [arg_values]. This pushes type errors "down" into each
       argument, rather than checking the function as a whole, giving better error messages (at
       the cost of more constraints to iterate through). *)
    let%bind arg_tys =
      List.map arg_values.value ~f:(fun arg_value ->
        let%bind.Result arg_ty, arg_constraints = infer arg_value ~env in
        let fresh_ty = Type.Var (Type.Var.create ()) in
        let fresh_type_equals_arg_ty : Annotations.t Constraint.t =
          Same_type
            (arg_ty, fresh_ty, [ Expression_should_have_type (arg_value, fresh_ty) ])
        in
        Ok ((arg_ty, fresh_type_equals_arg_ty :: arg_constraints), fresh_ty))
      |> Result.all
    in
    let return_ty : Type.t = Var (Type.Var.create ()) in
    let%bind fun_constraints =
      let fun_ty : Type.t = Fun (List.map arg_tys ~f:snd, return_ty) in
      check function_ fun_ty ~env ~loc:function_.loc
    in
    Ok
      ( return_ty
      , List.fold ~init:fun_constraints arg_tys ~f:(fun acc ((_, arg_constraints), _) ->
          merge acc arg_constraints) )
  | Let { name; value; in_ } ->
    (* [let v = x in y] should be equivalent to [(fun v -> y) x] *)
    infer
      { expr with
        desc =
          Apply
            ( { desc = Lambda ([ name ], in_); loc = expr.loc }
            , { value = [ value ]; loc = value.loc } )
      }
      ~env
  | Tuple elems ->
    let%bind types = List.map elems ~f:(infer ~env) |> Result.all in
    let types, out = List.unzip types in
    let ty : Type.t = Tuple types in
    Ok (ty, merge_list out)
  | Construct (constructor_name, arg) -> infer_constructor constructor_name arg ~env
  | Record fields -> infer_record fields ~env ~loc:expr.loc
  | Match { scrutinee; cases } ->
    (* FIXME: check match completeness *)
    let%bind scrutinee_ty, scrutinee_constraints = infer scrutinee ~env in
    let%bind body_ty, body_constraints =
      List.fold_result
        cases
        ~init:(None, empty)
        ~f:(fun (expected_body_type, infer_constraints) (pattern, body) ->
          let%bind pattern_constraints, env = check_pattern pattern scrutinee_ty ~env in
          let%bind body_ty, body_constraints =
            match expected_body_type with
            | None -> infer body ~env
            | Some expected_type ->
              let%bind constraints = check body expected_type ~env ~loc:body.loc in
              Ok (expected_type, constraints)
          in
          Ok
            ( Some body_ty
            , merge_list [ infer_constraints; pattern_constraints; body_constraints ] ))
    in
    let body_ty =
      (* The body type should be defined, since [cases] should never be empty. *)
      Option.value_exn ~message:"Internal compiler error" body_ty
    in
    Ok (body_ty, merge scrutinee_constraints body_constraints)

and infer_record fields ~env ~loc =
  let open Result.Let_syntax in
  let%bind type_name =
    (* Arbitrarily choose the first field as the "representative" to pick a type for this record. *)
    Env.field env (List.hd_exn fields |> fst).value ~loc
  in
  let%bind { args = type_args; shape; loc = _ } =
    Env.type_declaration env type_name ~loc
  in
  (* Make fresh type variables for the args *)
  let type_args = List.map type_args ~f:(fun old -> old, Type.Var (Type.Var.create ())) in
  let type_arg_mapping = Type.Var.Map.of_alist_exn type_args in
  let type_args = List.map type_args ~f:snd in
  let%bind field_types =
    match shape with
    | Record { fields = field_types; id = _ } -> Ok field_types
    | _ ->
      (* It's a bug for this type not to be a variant. We don't need to worry about aliases, since
         ; loc  = _    [Env.constructor] always points at the original declaration. *)
      failwith "Internal Compiler Error"
  in
  let fields = List.sort fields ~compare:[%compare: Field_name.t Located.t * _] in
  let field_types =
    List.sort field_types ~compare:[%compare: Field_name.t Located.t * _]
  in
  let%bind result =
    match
      List.map2
        fields
        field_types
        ~f:(fun (field_name, field_value) (expected_field_name, expected_field_type) ->
          let%bind `field_names_match =
            if Field_name.equal field_name.value expected_field_name.value
            then Ok `field_names_match
            else
              (* FIXME: Better error message could include "did you mean", error should point at the field
                 name in the expression, possibly with an "info" diagnostic pointing at the original
                 definition.

                 Error message could also include a reference to why we thought this record had this type
                 (i.e. because of the arbitrarily-chosen field [List.hd_exn fields]). *)
              Type_error.error_string
                ~loc:field_name.loc
                [%string
                  "Record field name mismatch: %{field_name.value#Field_name} does not \
                   match expected name %{expected_field_name.value#Field_name}"]
          in
          let expected_field_type =
            Type.subst expected_field_type ~replacements:type_arg_mapping
          in
          check field_value expected_field_type ~env ~loc:field_value.loc)
    with
    | Unequal_lengths ->
      let expected_fields =
        List.map field_types ~f:(fun ({ value; _ }, _) -> value) |> Field_name.Set.of_list
      in
      let specified_fields =
        List.map fields ~f:(fun ({ value; _ }, _) -> value) |> Field_name.Set.of_list
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
      Type_error.error_string ~loc error_message
    | Ok result -> Result.all result
  in
  Ok (Type.Apply ({ value = type_name; loc }, type_args), merge_list result)

and infer_constructor constructor_name arg ~env =
  let open Result.Let_syntax in
  let%bind type_name =
    Env.constructor env constructor_name.value ~loc:constructor_name.loc
  in
  let%bind { args = type_args; shape; loc = type_decl_loc } =
    Env.type_declaration env type_name ~loc:constructor_name.loc
  in
  (* Make fresh type variables for the args *)
  let type_args = List.map type_args ~f:(fun old -> old, Type.Var (Type.Var.create ())) in
  let type_arg_mapping = Type.Var.Map.of_alist_exn type_args in
  let type_args = List.map type_args ~f:snd in
  let%bind constructors =
    match shape with
    | Variant { constructors; id = _ } -> Ok constructors
    | _ ->
      (* It's a bug for this type not to be a variant. We don't need to worry about aliases, since
         [Env.constructor] always points at the original declaration. *)
      failwith "Internal Compiler Error"
  in
  let constructor =
    (* It's a bug for this constructor not to exist in the variant. *)
    List.Assoc.find_exn
      constructors
      constructor_name
      ~equal:(Located.value_equal ~f:Constructor.equal)
  in
  let%bind out =
    match constructor, arg with
    | None, None ->
      (* No constraints *)
      Ok empty
    | Some constructor_typ, Some value ->
      let constructor_typ = Type.subst constructor_typ ~replacements:type_arg_mapping in
      let%bind constraints = check value constructor_typ ~env ~loc:value.loc in
      Ok constraints
    | None, Some arg ->
      (* FIXME: this should point at the location of [value]. *)
      Type_error.error_string
        ~loc:arg.loc
        [%string
          "Arguments were provided to constructor %{constructor_name.value#Constructor}, \
           but none were expected."]
    | Some _, None ->
      (* FIXME: better error message would include the expected type. We'll need to move more of the
         pretty printing logic into this library.

         This should point at the location of the constructor. Ideally the diagnostic should also
         refer to the constructor in the original type declaration too. *)
      Type_error.error_string
        ~loc:constructor_name.loc
        [%string
          "Arguments of were expected for constructor \
           %{constructor_name.value#Constructor}, but none were provided."]
  in
  Ok (Type.Apply ({ value = type_name; loc = type_decl_loc }, type_args), out)

and check (expr : Expression.t) (expected_ty : Type.t) ~env ~loc : (t, _) result =
  let open Result.Let_syntax in
  match expr.desc, expected_ty with
  | Const (Int _), Intrinsic Int -> Ok empty
  | Const (String _), Intrinsic String -> Ok empty
  | Lambda (args, body), Fun (arg_types, result) ->
    let%bind env =
      match
        List.fold2 args arg_types ~init:env ~f:(fun env arg_name arg_type ->
          Env.with_var env arg_name (Type.Poly.mono arg_type))
      with
      | Unequal_lengths ->
        let expected_num_args = List.length arg_types in
        let num_args = List.length args in
        Type_error.error_string
          ~loc
          [%string
            "Function call expected %{expected_num_args#Int} args, but %{num_args#Int} \
             were provided."]
      | Ok env -> Ok env
    in
    check body result ~env ~loc:body.loc
  | _, expected_ty ->
    let%bind ty, constraints = infer expr ~env in
    Ok
      (add
         constraints
         (Same_type (ty, expected_ty, [ Expression_should_have_type (expr, expected_ty) ])))

and check_pattern (pattern : Pattern.t) expected_ty ~env : (t * Env.t, _) result =
  let open Result.Let_syntax in
  match pattern.desc with
  | Var name ->
    let env = Env.with_var env name.value (Type.Poly.mono expected_ty) in
    Ok (empty, env)
  | Construct (constructor_name, arg) ->
    (* FIXME: share this code with [check_constructor] *)
    let%bind type_name =
      Env.constructor env constructor_name.value ~loc:constructor_name.loc
    in
    let%bind { args = type_args; shape; loc = _ } =
      Env.type_declaration env type_name ~loc:constructor_name.loc
    in
    (* Make fresh type variables for the args *)
    let type_args =
      List.map type_args ~f:(fun old -> old, Type.Var (Type.Var.create ()))
    in
    let type_arg_mapping = Type.Var.Map.of_alist_exn type_args in
    let type_args = List.map type_args ~f:snd in
    let%bind constructors =
      match shape with
      | Variant { constructors; id = _ } -> Ok constructors
      | _ ->
        (* It's a bug for this type not to be a variant. We don't need to worry about aliases, since
           [Env.constructor] always points at the original declaration. *)
        failwith "Internal Compiler Error"
    in
    let arg_type =
      (* It's a bug for this constructor not to exist in the variant. *)
      List.Assoc.find_exn
        constructors
        constructor_name
        ~equal:(Located.value_equal ~f:Constructor.equal)
    in
    (match arg_type, arg with
     | None, None ->
       Ok
         ( singleton
             (Same_type
                ( expected_ty
                , Apply ({ value = type_name; loc = constructor_name.loc }, type_args)
                , [ Pattern_should_have_type (pattern, expected_ty) ] ))
         , env )
     | Some arg_type, Some arg_pattern ->
       let arg_type = Type.subst arg_type ~replacements:type_arg_mapping in
       let%bind constraints, env = check_pattern arg_pattern arg_type ~env in
       Ok
         ( add
             constraints
             (Same_type
                ( expected_ty
                , Apply ({ value = type_name; loc = constructor_name.loc }, type_args)
                , [ Pattern_should_have_type (pattern, expected_ty) ] ))
         , env )
     | Some _, None ->
       (* FIXME: this should produce an error that points to the (argument-less) pattern, with an info
          annotation pointing to the original type declaration. *)
       Type_error.error_string
         ~loc:constructor_name.loc
         [%string
           "Constructor %{constructor_name.value#Constructor} expects an argument, but \
            none was provided."]
     | None, Some _ ->
       (* FIXME: this should produce an error that points to the pattern arguments, with an info
          annotation pointing to the original type declaration. *)
       Type_error.error_string
         ~loc:constructor_name.loc
         [%string
           "Constructor %{constructor_name.value#Constructor} does not expect an \
            argument, but one was provided."])
  | Tuple patterns ->
    let type_vars =
      List.map patterns.value ~f:(fun pattern -> pattern, Type.Var.create ())
    in
    let%bind env, nested_constraints =
      List.fold_result
        type_vars
        ~init:(env, empty)
        ~f:(fun (env, constraints) (pattern, type_var) ->
          let%bind constraints', env = check_pattern pattern (Var type_var) ~env in
          Ok (env, merge constraints constraints'))
    in
    Ok
      ( add
          nested_constraints
          (Same_type
             ( expected_ty
             , Tuple (List.map type_vars ~f:(fun (_, type_var) : Type.t -> Var type_var))
             , [ Pattern_should_have_type (pattern, expected_ty) ] ))
      , env )
;;

module For_testing = struct
  let check_pattern = check_pattern
end
