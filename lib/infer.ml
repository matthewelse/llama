open! Core
open! Import

let subst_env (env : Type.Poly.t Ident.Map.t) ~replacements =
  Map.map env ~f:(Type.Poly.subst ~replacements)
;;

let rec type_of
  (expr : Expression.t)
  (env : Type.Poly.t Ident.Map.t)
  (tyenv : Type.Constructor.t Type_name.Map.t)
  (constructors : Type_name.t Constructor.Map.t)
  (fields : Type_name.t Field_name.Map.t)
  : (Type.t Type.Var.Map.t * Type.t) Or_error.t
  =
  let open Or_error.Let_syntax in
  match expr with
  | Var name ->
    let%bind poly_type =
      Map.find env name
      |> Or_error.of_option
           ~error:(Error.of_string [%string "Unbound variable [%{name#Ident}]"])
    in
    let type_ = Type.Poly.init poly_type in
    Ok (Type.Var.Map.empty, type_)
  | Apply (l, args) ->
    let%bind s, type_l = type_of l env tyenv constructors fields in
    let env = subst_env env ~replacements:s in
    let%bind s, arg_types =
      List.fold_result args ~init:(s, []) ~f:(fun (replacements, arg_types) arg ->
        let%bind replacements', ty = type_of arg env tyenv constructors fields in
        Ok
          ( Map.merge_skewed replacements replacements' ~combine:(fun ~key:_ _ t2 -> t2)
          , ty :: arg_types ))
    in
    let type_l = Type.subst type_l ~replacements:s in
    (* Unify *)
    let result_var = Type.Var (Type.Var.create ()) in
    let s = Type.unify' type_l (Fun (arg_types, result_var)) ~tyenv ~acc:s in
    Ok (s, Type.subst result_var ~replacements:s)
  | Lambda (args, body) ->
    let name_vars = List.map args ~f:(fun name -> name, Type.Var.create ()) in
    let%bind s, body_type =
      type_of
        body
        (List.fold name_vars ~init:env ~f:(fun env (name, var) ->
           Map.set env ~key:name ~data:{ ty = Var var; quantifiers = Type.Var.Set.empty }))
        tyenv
        constructors
        fields
    in
    let body_type = Type.subst body_type ~replacements:s in
    Ok (s, Type.Fun (List.map name_vars ~f:(fun (_, var) -> Type.Var var), body_type))
  | Let { name; value; in_ } ->
    let%bind s1, t1 = type_of value env tyenv constructors fields in
    let generalized_type = Type.generalize t1 ~env in
    let env = Map.set (subst_env env ~replacements:s1) ~key:name ~data:generalized_type in
    type_of in_ env tyenv constructors fields
  | Const c -> Ok (Type.Var.Map.empty, Intrinsic (Expression.Const.intrinsic_type c))
  | Tuple ts ->
    let%bind replacements, types =
      List.fold_result
        ts
        ~init:(Type.Var.Map.empty, [])
        ~f:(fun (replacements, types) t ->
          let%bind replacements', ty = type_of t env tyenv constructors fields in
          Ok
            ( Map.merge_skewed replacements replacements' ~combine:(fun ~key:_ _ t2 -> t2)
            , ty :: types ))
    in
    Ok (replacements, Type.Tuple types)
  | Construct (constructor, arg) ->
    let%bind type_name =
      Map.find constructors constructor
      |> Or_error.of_option
           ~error:
             (Error.of_string [%string "Unbound constructor %{constructor#Constructor}"])
    in
    let these_constructors, args =
      let%tydi { shape; args } = Map.find_exn tyenv type_name in
      match shape with
      | Variant { constructors; _ } -> constructors, args
      | _ ->
        (* This should be an internal compiler error *)
        assert false
    in
    let type_args = List.map args ~f:(fun _ -> Type.Var (Type.Var.create ())) in
    let subst = List.zip_exn args type_args |> Type.Var.Map.of_alist_exn in
    let constructor_arg_typ =
      List.Assoc.find_exn these_constructors ~equal:Constructor.equal constructor
      |> Option.map ~f:(Type.subst ~replacements:subst)
    in
    (match arg with
     | None ->
       let%bind () =
         if Option.is_none constructor_arg_typ
         then Ok ()
         else
           Or_error.error_string
             "Constructor %{constructor#Constructor} is expected to take an argument, \
              but none was provided."
       in
       Ok (Type.Var.Map.empty, Type.Apply (type_name, type_args))
     | Some arg ->
       let%bind constructor_arg_typ =
         constructor_arg_typ
         |> Or_error.of_option
              ~error:
                (Error.of_string
                   [%string
                     "Constructor %{constructor#Constructor} does not take an argument"])
         |> Or_error.map ~f:(Type.subst ~replacements:subst)
       in
       let%bind s, arg_type = type_of arg env tyenv constructors fields in
       let s =
         Type.unify'
           arg_type
           (Type.subst constructor_arg_typ ~replacements:s)
           ~tyenv
           ~acc:s
       in
       let result_type =
         Type.Apply (type_name, type_args) |> Type.subst ~replacements:s
       in
       Ok (s, result_type))
;;

let type_of expr env tyenv constructors fields =
  let%bind.Or_error tvenv, ty = type_of expr env tyenv constructors fields in
  let type_vars =
    Map.filter tvenv ~f:(function
      | Var _ -> false
      | _ -> true)
    |> Map.key_set
  in
  let free_vars_in_type = Type.free_type_vars ty in
  let unsubstituted_vars = Set.inter free_vars_in_type type_vars in
  (* If there are vars in the type that haven't been substituted, return an error *)
  if Set.is_empty unsubstituted_vars
  then Ok ty
  else
    raise_s
      [%message "Unsubstituted vars in result type" (expr : Expression.t) (ty : Type.t)]
;;

let type_of_let_binding t ~env ~tyenv ~constructors ~fields =
  let%bind.Or_error ty = type_of t env tyenv constructors fields in
  Ok (Type.generalize ty ~env)
;;

let type_ast (ast : Ast.t) =
  let env = Ident.Map.empty in
  let tyenv = Type_name.Map.empty in
  let constructors = Constructor.Map.empty in
  let fields = Field_name.Map.empty in
  List.fold_result
    ast
    ~init:(env, tyenv, constructors, fields)
    ~f:(fun (env, tyenv, constructors, fields) structure_item ->
      match structure_item with
      | Let { name; value } ->
        let%bind.Or_error ty =
          type_of_let_binding value ~env ~tyenv ~constructors ~fields
        in
        let env = Map.set env ~key:name ~data:ty in
        Ok (env, tyenv, constructors, fields)
      | Intrinsic { name; type_; intrinsic = _ } ->
        (* Surprisingly, the actual intrinsic used isn't that important for type checking. We trust
           the type provided by the standard library. *)
        let type_ = Type.Poly.of_ast type_ ~var_mapping:String.Map.empty in
        let env = Map.set env ~key:name ~data:type_ in
        Ok (env, tyenv, constructors, fields)
      | Type_declaration { name = type_name; type_params; type_shape } ->
        let type_params =
          List.map type_params ~f:(fun name -> name, Type.Var.create ())
        in
        let type_var_mapping =
          String.Map.of_alist_reduce type_params ~f:(fun _ most_recent -> most_recent)
        in
        let shape, constructors, fields =
          match type_shape with
          | Alias ty ->
            let ty = Type.of_ast ty ~var_mapping:type_var_mapping in
            Type.Constructor.Shape.Alias ty, constructors, fields
          | Record { fields = record_fields } ->
            let record_fields =
              List.map record_fields ~f:(fun (name, ty) ->
                name, Type.of_ast ty ~var_mapping:type_var_mapping)
            in
            let shape : Type.Constructor.Shape.t =
              Record { fields = record_fields; id = Type.Id.create () }
            in
            ( shape
            , constructors
            , List.fold record_fields ~init:fields ~f:(fun fields (name, _) ->
                Map.add_exn fields ~key:name ~data:type_name) )
          | Variant { constructors = variant_constructors } ->
            let variant_constructors =
              List.map variant_constructors ~f:(fun (name, ty) ->
                name, Option.map ty ~f:(Type.of_ast ~var_mapping:type_var_mapping))
            in
            let shape : Type.Constructor.Shape.t =
              Variant { constructors = variant_constructors; id = Type.Id.create () }
            in
            ( shape
            , List.fold
                variant_constructors
                ~init:constructors
                ~f:(fun constructors (name, _) ->
                  Map.add_exn constructors ~key:name ~data:type_name)
            , fields )
        in
        let constructor : Type.Constructor.t =
          { args = List.map ~f:snd type_params; shape }
        in
        let tyenv = Map.set tyenv ~key:type_name ~data:constructor in
        Ok (env, tyenv, constructors, fields))
;;
