open! Core
open! Import

module Annotation = struct
  type t =
    | Expression_should_have_type of Expression.t * Type.t
    | Pattern_should_have_type of Pattern.t * Type.t
    | Var_requires_type_class of Ident.t Located.t * Type_class_name.t
  [@@deriving sexp_of]

  let loc t =
    match t with
    | Expression_should_have_type (e, _) -> Expression.loc e
    | Pattern_should_have_type (p, _) -> Pattern.loc p
    | Var_requires_type_class ({ loc; _ }, _) -> loc
  ;;
end

module Constraint = struct
  type 'a t =
    | Same_type of Type.t * Type.t * 'a
    | Implements_type_class of Type_class_name.t * Type.t * 'a
  [@@deriving sexp_of]
end

module Annotations = struct
  (* FIXME melse: factor out [Nonempty_list.t] *)
  type t = ( :: ) of Annotation.t * Annotation.t list [@@deriving sexp_of]

  let primary_loc (hd :: _) = Annotation.loc hd
end

type t = Annotations.t Constraint.t list [@@deriving sexp_of]

let add (t : t) c = c :: t
let singleton c : t = [ c ]
let empty = []
let merge t1 t2 = t1 @ t2
let merge_list ts = List.fold ts ~init:empty ~f:merge
let to_list t = t

module Gen_out = struct
  type 'ast t =
    { constraints : Annotations.t Constraint.t list
    ; typed_ast : 'ast
    }

  let unzip ts =
    List.map ts ~f:(fun { constraints; typed_ast } -> constraints, typed_ast)
    |> List.unzip
  ;;
end

let rec check_pattern (pattern : Ast.Pattern.t) expected_ty ~env
  : Typed_ast.Pattern.t Gen_out.t * Env.t
  =
  match pattern with
  | Var (name, _) ->
    let env = Env.with_var env name (Type.Poly.mono expected_ty) in
    { constraints = empty; typed_ast = Var (name, ()) }, env
  | Construct (((constructor_name, constructor_loc), arg), _) ->
    (* FIXME: share this code with [check_constructor] *)
    let type_name = Env.constructor_exn env constructor_name ~loc:constructor_loc in
    let%tydi { args = type_args; shape; loc = _ } =
      Env.type_declaration_exn env type_name ~loc:constructor_loc
    in
    (* Make fresh type variables for the args *)
    let type_args =
      List.map type_args ~f:(fun old -> old, Type.var (Type.Var.create ()))
    in
    let type_arg_mapping = Type.Var.Map.of_alist_exn type_args in
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
       ( { constraints =
             singleton
               (Same_type
                  ( expected_ty
                  , Apply (((type_name, `Position constructor_loc), type_args), ())
                  , [ Pattern_should_have_type (pattern, expected_ty) ] ))
         ; typed_ast = Construct (((constructor_name, ()), None), ())
         }
       , env )
     | Some arg_type, Some arg_pattern ->
       let arg_type = Type.subst arg_type ~replacements:type_arg_mapping in
       let%tydi { constraints; typed_ast }, env =
         check_pattern arg_pattern arg_type ~env
       in
       ( { constraints =
             add
               constraints
               (Same_type
                  ( expected_ty
                  , Apply (((type_name, `Position constructor_loc), type_args), ())
                  , [ Pattern_should_have_type (pattern, expected_ty) ] ))
         ; typed_ast = Construct (((constructor_name, ()), Some typed_ast), ())
         }
       , env )
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
    let type_vars = List.map patterns ~f:(fun pattern -> pattern, Type.Var.create ()) in
    let (env, nested_constraints), typed_asts =
      List.fold_map
        type_vars
        ~init:(env, empty)
        ~f:(fun (env, constraints) (pattern, type_var) ->
          let%tydi { constraints = constraints'; typed_ast }, env =
            check_pattern pattern (Var (type_var, ())) ~env
          in
          (env, merge constraints constraints'), typed_ast)
    in
    ( { constraints =
          add
            nested_constraints
            (Same_type
               ( expected_ty
               , Tuple
                   ( List.map type_vars ~f:(fun (_, type_var) : Type.t ->
                       Var (type_var, ()))
                   , () )
               , [ Pattern_should_have_type (pattern, expected_ty) ] ))
      ; typed_ast = Tuple (typed_asts, ())
      }
    , env )
;;

let rec infer (expr : Expression.t) ~env : Typed_ast.Expression.t Gen_out.t * Type.t =
  match expr with
  | Const (Int value, _) ->
    { constraints = empty; typed_ast = Const (Int value, ()) }, Type.intrinsic Int
  | Const (String value, _) ->
    { constraints = empty; typed_ast = Const (String value, ()) }, Type.intrinsic String
  | Var (v, loc) ->
    let poly_type = Env.value_exn env v ~loc in
    let type_, constraints = Type.Poly.init poly_type in
    let constraints =
      List.map constraints ~f:(fun { type_class; arg } ->
        Constraint.Implements_type_class
          ( type_class
          , arg
          , ([ Var_requires_type_class ({ value = v; loc }, type_class) ] : Annotations.t)
          ))
    in
    { constraints; typed_ast = Var (v, ()) }, type_
  | Lambda ((args, body), _) ->
    let arg_types = List.map args ~f:(fun name -> fst name, Type.Var.create ()) in
    let env = Env.with_vars env arg_types in
    let%tydi { constraints = body_constraints; typed_ast = body_typed_ast }, body_type =
      infer body ~env
    in
    ( { constraints = body_constraints
      ; typed_ast =
          Lambda
            ( ( List.map arg_types ~f:(fun (name, var) -> name, Type.var var)
              , body_typed_ast )
            , () )
      }
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
    let fresh_arg_tys = List.map arg_values ~f:(fun _ -> Type.var (Type.Var.create ())) in
    let return_ty = Type.var (Type.Var.create ()) in
    let fun_ty : Type.t = Fun ((fresh_arg_tys, return_ty), ()) in
    let%tydi { constraints = fun_constraints; typed_ast = fun_ast } =
      check function_ fun_ty ~env
    in
    let arg_tys =
      List.map2_exn arg_values fresh_arg_tys ~f:(fun arg_value fresh_ty ->
        let%tydi { constraints = arg_constraints; typed_ast = arg_ast }, arg_ty =
          infer arg_value ~env
        in
        let fresh_type_equals_arg_ty : Annotations.t Constraint.t =
          Same_type
            ( arg_ty
            , fresh_ty
            , [ Expression_should_have_type (arg_value, fresh_ty)
              ; Expression_should_have_type (function_, fun_ty)
              ] )
        in
        arg_ast, arg_ty, fresh_type_equals_arg_ty :: arg_constraints, fresh_ty)
    in
    ( { constraints =
          List.fold
            ~init:fun_constraints
            arg_tys
            ~f:(fun acc (_, _, arg_constraints, _) -> merge acc arg_constraints)
      ; typed_ast = Apply ((fun_ast, List.map arg_tys ~f:(fun (c, _, _, _) -> c)), ())
      }
    , return_ty )
  | Let { name; value; in_; annotation = loc } ->
    (* [let v = x in y] should be equivalent to [(fun v -> y) x] *)
    infer (Apply ((Lambda (([ name ], in_), loc), [ value ]), loc)) ~env
  | Tuple (elems, _) ->
    let types = List.map elems ~f:(infer ~env) in
    let out, types = List.unzip types in
    let out_constraints, out_asts = Gen_out.unzip out in
    let ty : Type.t = Tuple (types, ()) in
    { constraints = merge_list out_constraints; typed_ast = Tuple (out_asts, ()) }, ty
  | Construct ((constructor_name, arg), _) -> infer_constructor constructor_name arg ~env
  | Record (fields, loc) -> infer_record fields ~env ~loc
  | Match { scrutinee; cases; annotation = loc } ->
    (* FIXME: check match completeness *)
    let%tydi ( { constraints = scrutinee_constraints; typed_ast = scrutinee_typed_ast }
             , scrutinee_ty )
      =
      infer scrutinee ~env
    in
    let%tydi (body_ty, body_constraints), typed_cases =
      List.fold_map
        cases
        ~init:(None, empty)
        ~f:(fun (expected_body_type, infer_constraints) (pattern, body) ->
          let%tydi { constraints = pattern_constraints; typed_ast = pattern_ast }, env =
            check_pattern pattern scrutinee_ty ~env
          in
          let%tydi { constraints = body_constraints; typed_ast = body_ast }, body_ty =
            match expected_body_type with
            | None -> infer body ~env
            | Some expected_type ->
              let out = check body expected_type ~env in
              out, expected_type
          in
          ( ( Some body_ty
            , merge_list [ infer_constraints; pattern_constraints; body_constraints ] )
          , (pattern_ast, body_ast) ))
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
    ( { constraints = merge scrutinee_constraints body_constraints
      ; typed_ast =
          Match { scrutinee = scrutinee_typed_ast; cases = typed_cases; annotation = () }
      }
    , body_ty )
  | TFun _ | TApply _ -> .

and infer_record fields ~env ~loc =
  let type_name =
    (* Arbitrarily choose the first field as the "representative" to pick a type for this record. *)
    Env.field_exn env (List.hd_exn fields |> fst |> fst) ~loc
  in
  let%tydi { args = type_args; shape; loc = _ } =
    Env.type_declaration_exn env type_name ~loc
  in
  (* Make fresh type variables for the args *)
  let type_args =
    List.map type_args ~f:(fun old -> old, Type.Var (Type.Var.create (), ()))
  in
  let type_arg_mapping = Type.Var.Map.of_alist_exn type_args in
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
          field_name, check field_value expected_field_type ~env)
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
  let constraints, typed_fields =
    List.map result ~f:(fun (name, { constraints; typed_ast }) ->
      constraints, ((name, ()), typed_ast))
    |> List.unzip
  in
  ( { constraints = merge_list constraints; typed_ast = Record (typed_fields, ()) }
  , Type.Apply (((type_name, `Position loc), type_args), ()) )

and infer_constructor (constructor_name, constructor_name_loc) arg ~env =
  let type_name = Env.constructor_exn env constructor_name ~loc:constructor_name_loc in
  let%tydi { args = type_args; shape; loc = type_decl_loc } =
    Env.type_declaration_exn env type_name ~loc:constructor_name_loc
  in
  (* Make fresh type variables for the args *)
  let type_args =
    List.map type_args ~f:(fun old -> old, Type.Var (Type.Var.create (), ()))
  in
  let type_arg_mapping = Type.Var.Map.of_alist_exn type_args in
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
  let out : Typed_ast.Expression.t Gen_out.t =
    match constructor, arg with
    | None, None ->
      (* No constraints *)
      { constraints = empty; typed_ast = Construct (((constructor_name, ()), None), ()) }
    | Some constructor_typ, Some value ->
      let constructor_typ = Type.subst constructor_typ ~replacements:type_arg_mapping in
      let out = check value constructor_typ ~env in
      { out with
        typed_ast = Construct (((constructor_name, ()), Some out.typed_ast), ())
      }
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

and check (expr : Expression.t) (expected_ty : Type.t) ~env
  : Typed_ast.Expression.t Gen_out.t
  =
  match expr, expected_ty with
  | Lambda ((args, body), loc), Fun ((arg_types, result), _) ->
    let env =
      match
        List.fold2 args arg_types ~init:env ~f:(fun env (arg_name, _) arg_type ->
          Env.with_var env arg_name (Type.Poly.mono arg_type))
      with
      | Unequal_lengths ->
        let expected_num_args = List.length arg_types in
        let num_args = List.length args in
        Reporter.fatalf
          ~loc:(Asai.Range.of_lex_range loc)
          Type_error
          "Function call expected %d args, but %d were provided."
          expected_num_args
          num_args
      | Ok env -> env
    in
    check body result ~env
  | _, expected_ty ->
    let%tydi { constraints; typed_ast }, ty = infer expr ~env in
    { typed_ast
    ; constraints =
        add
          constraints
          (Same_type (ty, expected_ty, [ Expression_should_have_type (expr, expected_ty) ]))
    }
;;

module For_testing = struct
  let check_pattern = check_pattern
end
