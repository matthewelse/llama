open! Core
open! Import

module Env = struct
  type t =
    { values : Type.Poly.t Ident.Map.t
    ; type_declarations : Type.Constructor.t Type_name.Map.t
    ; constructors : Type_name.t Constructor.Map.t
    ; fields : Type_name.t Field_name.Map.t
    }
  [@@deriving sexp_of]

  let empty =
    { values = Ident.Map.empty
    ; type_declarations = Type_name.Map.empty
    ; constructors = Constructor.Map.empty
    ; fields = Field_name.Map.empty
    }
  ;;

  let field t name =
    Map.find t.fields name
    |> Or_error.of_option
         ~error:(Error.of_string [%string "Unbound field [%{name#Field_name}]"])
  ;;

  let constructor t name =
    Map.find t.constructors name
    |> Or_error.of_option
         ~error:(Error.of_string [%string "Unbound constructor %{name#Constructor}"])
  ;;

  let value t name =
    Map.find t.values name
    |> Or_error.of_option
         ~error:(Error.of_string [%string "Unbound variable [%{name#Ident}]"])
  ;;

  let subst t ~replacements =
    { t with values = Map.map t.values ~f:(Type.Poly.subst ~replacements) }
  ;;

  let with_fields t fields ~type_name =
    let%bind.Or_error fields =
      List.fold_result fields ~init:t.fields ~f:(fun fields (name, _) ->
        match Map.add fields ~key:name ~data:type_name with
        | `Ok fields -> Ok fields
        | `Duplicate ->
          Error (Error.of_string [%string "Duplicate field [%{name#Field_name}]"]))
    in
    Ok { t with fields }
  ;;

  let with_constructors t constructors ~type_name =
    { t with
      constructors =
        List.fold constructors ~init:t.constructors ~f:(fun fields (name, _) ->
          Map.add_exn fields ~key:name ~data:type_name)
    }
  ;;

  let with_type_declaration t name ty =
    { t with type_declarations = Map.add_exn t.type_declarations ~key:name ~data:ty }
  ;;

  let with_vars t vars =
    let values =
      List.fold vars ~init:t.values ~f:(fun env (name, var) ->
        Map.set env ~key:name ~data:{ ty = Var var; quantifiers = Type.Var.Set.empty })
    in
    { t with values }
  ;;

  let with_var t name ty = { t with values = Map.set t.values ~key:name ~data:ty }
end

let debug = false

let rec type_of (expr : Expression.t) (env : Env.t)
  : (Type.t Type.Var.Map.t * Type.t) Or_error.t
  =
  let open Or_error.Let_syntax in
  if debug then eprint_s [%message "type_of" (expr : Expression.t) (env : Env.t)];
  let result =
    match expr with
    | Var name ->
      let%bind poly_type = Env.value env name in
      let type_ = Type.Poly.init poly_type in
      Ok (Type.Var.Map.empty, type_)
    | Apply (l, args) ->
      let%bind s, type_l = type_of l env in
      let env = Env.subst env ~replacements:s in
      let%bind s, arg_types =
        List.fold_result args ~init:(s, []) ~f:(fun (replacements, arg_types) arg ->
          let%bind replacements', ty = type_of arg env in
          Ok
            ( Map.merge_skewed replacements replacements' ~combine:(fun ~key:_ _ t2 -> t2)
            , ty :: arg_types ))
      in
      let type_l = Type.subst type_l ~replacements:s in
      (* Unify *)
      let result_var = Type.Var (Type.Var.create ()) in
      let s =
        Type.unify'
          type_l
          (Fun (arg_types, result_var))
          ~tyenv:env.type_declarations
          ~acc:s
      in
      Ok (s, Type.subst result_var ~replacements:s)
    | Lambda (args, body) ->
      let name_vars = List.map args ~f:(fun name -> name, Type.Var.create ()) in
      let%bind s, body_type = type_of body (Env.with_vars env name_vars) in
      let body_type = Type.subst body_type ~replacements:s in
      Ok (s, Type.Fun (List.map name_vars ~f:(fun (_, var) -> Type.Var var), body_type))
    | Let { name; value; in_ } ->
      let%bind s1, t1 = type_of value env in
      (* FIXME melse: Don't generalize types if they're values. *)
      let generalized_type = Type.generalize t1 ~env:env.values in
      let env = Env.with_var (Env.subst env ~replacements:s1) name generalized_type in
      type_of in_ env
    | Const c -> Ok (Type.Var.Map.empty, Intrinsic (Expression.Const.intrinsic_type c))
    | Tuple ts ->
      let%bind replacements, types =
        List.fold_result
          ts
          ~init:(Type.Var.Map.empty, [])
          ~f:(fun (replacements, types) t ->
            let%bind replacements', ty = type_of t env in
            Ok
              ( Map.merge_skewed replacements replacements' ~combine:(fun ~key:_ _ t2 ->
                  t2)
              , ty :: types ))
      in
      Ok (replacements, Type.Tuple (List.rev types))
    | Construct (constructor, arg) ->
      let%bind type_name = Env.constructor env constructor in
      let these_constructors, args =
        let%tydi { shape; args } = Map.find_exn env.type_declarations type_name in
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
         let%bind s, arg_type = type_of arg env in
         let s =
           Type.unify'
             arg_type
             (Type.subst constructor_arg_typ ~replacements:s)
             ~tyenv:env.type_declarations
             ~acc:s
         in
         let result_type =
           Type.Apply (type_name, type_args) |> Type.subst ~replacements:s
         in
         Ok (s, result_type))
    | Record fields ->
      (* 1. Infer the type of the record from the field name. This obviously isn't great,
         but whatever. *)
      let%bind first_field, _ =
        List.hd fields
        |> Or_error.of_option
             ~error:(Error.of_string "Record must have at least one field")
      in
      let%bind type_name = Env.field env first_field in
      (* 2. Get the shape of the type. *)
      let decl_fields, args =
        let%tydi { shape; args } = Map.find_exn env.type_declarations type_name in
        match shape with
        | Record { fields; _ } -> fields, args
        | _ ->
          (* This should be an internal compiler error: we should never have added a reference from
             the field name to the type if it wasn't a record. *)
          assert false
      in
      (* 3. Create a fresh type variable for each of the type arguments. *)
      let type_args = List.map args ~f:(fun _ -> Type.Var (Type.Var.create ())) in
      (* 4. Substitute the fresh type args for the original type args in the type of each field. *)
      let subst = List.zip_exn args type_args |> Type.Var.Map.of_alist_exn in
      let s =
        let decl_fields = List.sort decl_fields ~compare:[%compare: Field_name.t * _] in
        let fields = List.sort fields ~compare:[%compare: Field_name.t * _] in
        List.fold2_exn
          decl_fields
          fields
          ~init:Type.Var.Map.empty
          ~f:(fun acc (decl_field, typ) (field, expr_typ) ->
            (* FIXME: proper error handling *)
            assert (Field_name.equal decl_field field);
            let decl_type = Type.subst typ ~replacements:subst in
            (* 5. Unify the types of each field expression with the expected type of each field. *)
            let s, expr_type =
              (* FIXME: proper error handling *)
              Or_error.ok_exn @@ type_of expr_typ env
            in
            let s = Map.merge_skewed s acc ~combine:(fun ~key:_ _ t2 -> t2) in
            Type.unify'
              expr_type
              (Type.subst decl_type ~replacements:s)
              ~tyenv:env.type_declarations
              ~acc:s)
      in
      let result_type = Type.Apply (type_name, type_args) |> Type.subst ~replacements:s in
      Ok (s, result_type)
  in
  if debug
  then
    eprint_s
      [%message
        "type_of"
          (expr : Expression.t)
          (result : (Type.t Type.Var.Map.t * Type.t) Or_error.t)];
  result
;;

let type_of expr env =
  let%bind.Or_error tvenv, ty = type_of expr env in
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

let type_of_let_binding t env =
  let%bind.Or_error ty = type_of t env in
  Ok (Type.generalize ty ~env:env.values)
;;

let type_ast ?(env = Env.empty) (ast : Ast.t) =
  List.fold_result ast ~init:env ~f:(fun env structure_item ->
    match structure_item with
    | Let { name; value } ->
      let%bind.Or_error ty = type_of_let_binding value env in
      let env = Env.with_var env name ty in
      Ok env
    | Intrinsic { name; type_; intrinsic = _ } ->
      (* Surprisingly, the actual intrinsic used isn't that important for type checking. We trust
         the type provided by the standard library. *)
      let type_ = Type.Poly.of_ast type_ ~var_mapping:String.Map.empty in
      let env = Env.with_var env name type_ in
      Ok env
    | Type_declaration { name = type_name; type_params; type_shape } ->
      let type_params = List.map type_params ~f:(fun name -> name, Type.Var.create ()) in
      let type_var_mapping =
        String.Map.of_alist_reduce type_params ~f:(fun _ most_recent -> most_recent)
      in
      let%bind.Or_error shape, env =
        match type_shape with
        | Alias ty ->
          let ty = Type.of_ast ty ~var_mapping:type_var_mapping in
          Ok (Type.Constructor.Shape.Alias ty, env)
        | Record { fields = record_fields } ->
          let record_fields =
            List.map record_fields ~f:(fun (name, ty) ->
              name, Type.of_ast ty ~var_mapping:type_var_mapping)
          in
          let shape : Type.Constructor.Shape.t =
            Record { fields = record_fields; id = Type.Id.create () }
          in
          let%bind.Or_error env = Env.with_fields env record_fields ~type_name in
          Ok (shape, env)
        | Variant { constructors = variant_constructors } ->
          let variant_constructors =
            List.map variant_constructors ~f:(fun (name, ty) ->
              name, Option.map ty ~f:(Type.of_ast ~var_mapping:type_var_mapping))
          in
          let shape : Type.Constructor.Shape.t =
            Variant { constructors = variant_constructors; id = Type.Id.create () }
          in
          Ok (shape, Env.with_constructors env variant_constructors ~type_name)
      in
      let constructor : Type.Constructor.t =
        { args = List.map ~f:snd type_params; shape }
      in
      let env = Env.with_type_declaration env type_name constructor in
      Ok env)
;;
