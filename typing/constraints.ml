open! Core
open! Import

module Constraint = struct
  type t = Same_type of Type.t * Type.t * string [@@deriving sexp_of]
end

type t = Constraint.t list [@@deriving sexp_of]

let add (t : t) c = c :: t
let singleton c : t = [ c ]
let empty = []
let merge t1 t2 = t1 @ t2
let merge_list ts = List.fold ts ~init:empty ~f:merge
let to_list t = t

let rec infer (expr : Expression.t) ~env =
  let open Result.Let_syntax in
  let no_constraints ty : Type.t * t = ty, empty in
  match expr with
  | Const (Int _) -> Ok (no_constraints (Intrinsic Int))
  | Const (String _) -> Ok (no_constraints (Intrinsic String))
  | Var v ->
    let%bind poly_type = Env.value env v in
    let type_ = Type.Poly.init poly_type in
    Ok (no_constraints type_)
  | Lambda (args, body) ->
    let arg_types = List.map args ~f:(fun name -> name, Type.Var.create ()) in
    let env = Env.with_vars env arg_types in
    let%bind body_type, body_constraints = infer body ~env in
    Ok
      ( Type.Fun (List.map arg_types ~f:(fun (_, var) -> Type.var var), body_type)
      , body_constraints )
  | Apply (function_, args) ->
    let%bind args = List.map args ~f:(infer ~env) |> Result.all in
    let return_ty : Type.t = Var (Type.Var.create ()) in
    let fun_ty : Type.t = Fun (List.map args ~f:fst, return_ty) in
    let%bind fun_constraints = check function_ fun_ty ~env in
    Ok
      ( return_ty
      , List.fold ~init:fun_constraints args ~f:(fun acc (_, arg_constraints) ->
          merge acc arg_constraints) )
  | Let { name; value; in_ } ->
    (* [let v = x in y] should be equivalent to [(fun v -> y) x] *)
    infer (Apply (Lambda ([ name ], in_), [ value ])) ~env
  | Tuple elems ->
    let%bind types = List.map elems ~f:(infer ~env) |> Result.all in
    let types, out = List.unzip types in
    let ty : Type.t = Tuple types in
    Ok (ty, merge_list out)
  | Construct (constructor_name, arg) -> infer_constructor constructor_name arg ~env
  | Record fields -> infer_record fields ~env
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
              let%bind constraints = check body expected_type ~env in
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

and infer_record fields ~env =
  let open Result.Let_syntax in
  let%bind type_name =
    (* Arbitrarily choose the first field as the "representative" to pick a type for this record. *)
    Env.field env (List.hd_exn fields |> fst)
  in
  let%bind { args = type_args; shape } = Env.type_declaration env type_name in
  (* Make fresh type variables for the args *)
  let type_args = List.map type_args ~f:(fun old -> old, Type.Var (Type.Var.create ())) in
  let type_arg_mapping = Type.Var.Map.of_alist_exn type_args in
  let type_args = List.map type_args ~f:snd in
  let%bind field_types =
    match shape with
    | Record { fields = field_types; id = _ } -> Ok field_types
    | _ ->
      (* It's a bug for this type not to be a variant. We don't need to worry about aliases, since
         [Env.constructor] always points at the original declaration. *)
      failwith "Internal Compiler Error"
  in
  let fields = List.sort fields ~compare:[%compare: Field_name.t * _] in
  let field_types = List.sort field_types ~compare:[%compare: Field_name.t * _] in
  let%bind result =
    match
      List.map2
        fields
        field_types
        ~f:(fun (field_name, field_value) (expected_field_name, expected_field_type) ->
          let%bind `field_names_match =
            if Field_name.equal field_name expected_field_name
            then Ok `field_names_match
            else
              (* FIXME: Better error message could include "did you mean", error should point at the field
                 name in the expression, possibly with an "info" diagnostic pointing at the original
                 definition.

                 Error message could also include a reference to why we thought this record had this type
                 (i.e. because of the arbitrarily-chosen field [List.hd_exn fields]). *)
              Or_error.error_string
                [%string
                  "Record field name mismatch: %{field_name#Field_name} does not match \
                   expected name %{expected_field_name#Field_name}"]
          in
          let expected_field_type =
            Type.subst expected_field_type ~replacements:type_arg_mapping
          in
          check field_value expected_field_type ~env)
    with
    | Unequal_lengths ->
      let expected_fields = List.map field_types ~f:fst |> Field_name.Set.of_list in
      let specified_fields = List.map fields ~f:fst |> Field_name.Set.of_list in
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
      Or_error.error_string error_message
    | Ok result -> Result.all result
  in
  Ok (Type.Apply (type_name, type_args), merge_list result)

and infer_constructor constructor_name arg ~env =
  let open Result.Let_syntax in
  let%bind type_name = Env.constructor env constructor_name in
  let%bind { args = type_args; shape } = Env.type_declaration env type_name in
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
    List.Assoc.find_exn constructors constructor_name ~equal:Constructor.equal
  in
  let%bind out =
    match constructor, arg with
    | None, None ->
      (* No constraints *)
      Ok empty
    | Some constructor_typ, Some value ->
      let constructor_typ = Type.subst constructor_typ ~replacements:type_arg_mapping in
      let%bind constraints = check value constructor_typ ~env in
      Ok constraints
    | None, Some _ ->
      (* FIXME: this should point at the location of [value]. *)
      Or_error.error_string
        [%string
          "Arguments were provided to constructor %{constructor_name#Constructor}, but \
           none were expected."]
    | Some _, None ->
      (* FIXME: better error message would include the expected type. We'll need to move more of the
         pretty printing logic into this library.

         This should point at the location of the constructor. Ideally the diagnostic should also
         refer to the constructor in the original type declaration too. *)
      Or_error.error_string
        [%string
          "Arguments of were expected for constructor %{constructor_name#Constructor}, \
           but none were provided."]
  in
  Ok (Type.Apply (type_name, type_args), out)

and check (expr : Expression.t) (expected_ty : Type.t) ~env : (t, _) result =
  let open Result.Let_syntax in
  match expr, expected_ty with
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
        Or_error.error_string
          [%string
            "Function call expected %{expected_num_args#Int} args, but %{num_args#Int} \
             were provided."]
      | Ok env -> Ok env
    in
    check body result ~env
  | other, expected_ty ->
    let%bind ty, constraints = infer other ~env in
    Ok (add constraints (Same_type (ty, expected_ty, "check t1 t2")))

and check_pattern (pattern : Pattern.t) expected_ty ~env : (t * Env.t, _) result =
  let open Result.Let_syntax in
  match pattern with
  | Var name ->
    let env = Env.with_var env name (Type.Poly.mono expected_ty) in
    Ok (empty, env)
  | Construct (constructor_name, arg) ->
    (* FIXME: share this code with [check_constructor] *)
    let%bind type_name = Env.constructor env constructor_name in
    let%bind { args = type_args; shape } = Env.type_declaration env type_name in
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
      List.Assoc.find_exn constructors constructor_name ~equal:Constructor.equal
    in
    (match arg_type, arg with
     | None, None ->
       Ok
         ( singleton
             (Same_type
                (expected_ty, Apply (type_name, type_args), "pattern (construct: no args)"))
         , env )
     | Some arg_type, Some arg_pattern ->
       let arg_type = Type.subst arg_type ~replacements:type_arg_mapping in
       let%bind constraints, env = check_pattern arg_pattern arg_type ~env in
       Ok
         ( add
             constraints
             (Same_type
                ( expected_ty
                , Apply (type_name, type_args)
                , "pattern (construct: some args)" ))
         , env )
     | Some _, None ->
       (* FIXME: this should produce an error that points to the (argument-less) pattern, with an info
          annotation pointing to the original type declaration. *)
       Or_error.error_string
         [%string
           "Constructor %{constructor_name#Constructor} expects an argument, but none \
            was provided."]
     | None, Some _ ->
       (* FIXME: this should produce an error that points to the pattern arguments, with an info
          annotation pointing to the original type declaration. *)
       Or_error.error_string
         [%string
           "Constructor %{constructor_name#Constructor} does not expect an argument, but \
            one was provided."])
  | Tuple patterns ->
    let type_vars = List.map patterns ~f:(fun pattern -> pattern, Type.Var.create ()) in
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
             , "pattern (tuple)" ))
      , env )
;;

module For_testing = struct
  let check_pattern = check_pattern
end
