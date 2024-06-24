open! Core
open! Import

let maybe_generalize_expression_type expr ty ~(env : Env.t) =
  if Expression.is_syntactic_value expr
  then Type.generalize ty ~env:env.values
  else Type.Poly.mono ty
;;

let type_of_let_binding expr env =
  let%bind.Result ty, constraints = Constraints.infer expr ~env in
  (* FIXME melse: at some point we should generalize types. *)
  Ok (ty, constraints)
;;

let debug = false

let type_ast ?(env = Env.empty) (ast : Ast.t) =
  let open Result.Let_syntax in
  List.fold_result ast ~init:env ~f:(fun env structure_item ->
    match structure_item with
    | Let { name; value } ->
      if debug then print_endline [%string "======== typing %{name#Ident}"];
      if debug then print_s [%message "type_ast" (name : Ident.t) (value : Expression.t)];
      let%bind ty, constraints = type_of_let_binding value env in
      if debug then print_s [%message (ty : Type.t) (constraints : Constraints.t)];
      let solver = Solver.create () in
      let%bind env = Solver.solve solver constraints ~env in
      if debug then print_s [%message (solver : Solver.t)];
      let%bind ty = Solver.normalize_ty solver ty ~env in
      let ty = maybe_generalize_expression_type value ty ~env in
      if debug then print_s [%message (ty : Type.Poly.t)];
      let env = Env.with_var env name ty in
      if debug then print_s [%message (env : Env.t)];
      Ok env
    | Intrinsic { name; type_; intrinsic = _ } ->
      (* Surprisingly, the actual intrinsic used isn't that important for type checking. We trust
         the type provided by the standard library. *)
      let type_ = Type.Poly.of_ast type_ ~var_mapping:String.Map.empty in
      let env = Env.with_var env name type_ in
      Ok env
    | Type_declaration { name = { value = type_name; _ }; type_params; type_shape } ->
      let type_params =
        List.map type_params ~f:(fun { value = name; _ } -> name, Type.Var.create ())
      in
      let type_var_mapping =
        String.Map.of_alist_reduce type_params ~f:(fun _ most_recent -> most_recent)
      in
      let%bind.Result shape, env =
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
          let%bind.Result env = Env.with_fields env record_fields ~type_name in
          Ok (shape, env)
        | Variant { constructors = variant_constructors } ->
          let variant_constructors =
            List.map variant_constructors ~f:(fun (name, ty) ->
              name, Option.map ty ~f:(Type.of_ast ~var_mapping:type_var_mapping))
          in
          let shape : Type.Constructor.Shape.t =
            Variant { constructors = variant_constructors; id = Type.Id.create () }
          in
          let%bind env = Env.with_constructors env variant_constructors ~type_name in
          Ok (shape, env)
      in
      let constructor : Type.Constructor.t =
        { args = List.map ~f:snd type_params; shape }
      in
      let env = Env.with_type_declaration env type_name constructor in
      Ok env)
;;
