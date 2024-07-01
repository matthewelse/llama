open! Core
open! Import

let maybe_generalize_expression_type expr ty ~(env : Env.t) =
  if Expression.is_syntactic_value expr
  then Type.generalize ty ~env:env.values
  else Type.Poly.mono ty
;;

let type_ast ?env (ast : Ast.t) =
  let open Result.Let_syntax in
  let env = Option.value_or_thunk env ~default:Env.empty in
  List.fold_result ast ~init:env ~f:(fun env structure_item ->
    match structure_item with
    | Let { name = name, _; value; loc = _ } ->
      (* Assume that [name] is recursive. Create a fresh type variable to represent the type of
         this value. *)
      let env =
        let this_ty = Type.Var.create () in
        Env.with_var
          env
          name
          { body = Var (this_ty, ()); quantifiers = []; constraints = [] }
      in
      let%bind ty, constraints = Constraints.infer value ~env in
      (* Solve the constraints we've generated. *)
      let solver = Solver.create () in
      let%bind env = Solver.solve solver constraints ~env in
      let%bind ty = Solver.normalize_ty solver ty ~env in
      (* FIXME melse: Is this reasonable? We're removing [name] to ensure that its type vars get
         universally quantified.

         I think so: https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Typing_rule
         we treat [name] as a monotype inside [name], and a polytype afterwards. Seems
         reasonable. *)
      let env = Env.remove_var env name in
      let ty = maybe_generalize_expression_type value ty ~env in
      let%bind constraints = Solver.constraints solver ~env in
      let env =
        Env.with_var
          env
          name
          { ty with
            constraints =
              List.concat_map constraints ~f:(fun (type_class, constraints) ->
                List.map constraints ~f:(fun arg : Type.Constraint.t ->
                  { type_class; arg }))
          }
      in
      Ok env
    | Intrinsic { name = name, _; type_; intrinsic = _; loc = _ } ->
      (* Surprisingly, the actual intrinsic used isn't that important for type checking. We trust
         the type provided by the standard library. *)
      let type_ = Type.Poly.of_ast type_ ~var_mapping:String.Map.empty in
      let env = Env.with_var env name type_ in
      Ok env
    | Type_class_declaration
        { name = type_class_name, _; arg = arg, _; functions; constraints = _ } ->
      (* TODO melse: implement me! include use constraints *)
      let env =
        List.fold functions ~init:env ~f:(fun env { name; ty } ->
          let type_params =
            List.map (Ast.Type.free_type_vars ty) ~f:(fun name ->
              name, Type.Var.create ())
          in
          let var_mapping =
            String.Map.of_alist_reduce type_params ~f:(fun _ most_recent -> most_recent)
          in
          let arg : Type.t = Var (Map.find_exn var_mapping arg, ()) in
          let body = Type.of_ast ty ~var_mapping in
          Env.with_var
            env
            (fst name)
            Type.Poly.
              { body
              ; quantifiers = Type.free_type_vars body |> Set.to_list
              ; constraints = [ { type_class = type_class_name; arg } ]
              })
      in
      Ok env
    | Type_class_implementation
        { name = name, _
        ; for_ = (type_name, type_name_loc), args
        ; functions = _
        ; constraints
        } ->
      let type_arg =
        List.all_equal args ~equal:[%equal: string * _] |> Option.map ~f:fst
      in
      let type_arg = ref type_arg in
      let type_var = Type.Var.create () in
      let constraints =
        List.map constraints ~f:(fun { type_class = type_class, _; arg = arg, _ } ->
          (match !type_arg with
           | None -> type_arg := Some arg
           | Some type_arg -> assert (String.equal arg type_arg));
          type_class)
      in
      let tc : Type_class.Impl.t =
        { constraints
        ; type_class = name
        ; for_type =
            Apply
              ( ( (type_name, `Position type_name_loc)
                , List.map args ~f:(fun _ : Type.t -> Var (type_var, ())) )
              , () )
        }
      in
      Ok (Env.with_type_class_impl env tc)
    | Type_declaration { name = type_name, _; type_params; type_shape; loc } ->
      let type_params =
        List.map type_params ~f:(fun (name, _) -> name, Type.Var.create ())
      in
      let type_var_mapping =
        String.Map.of_alist_reduce type_params ~f:(fun _ most_recent -> most_recent)
      in
      let%bind.Result shape, env =
        match type_shape with
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
        { args = List.map ~f:snd type_params; shape; loc = `Position loc }
      in
      let env = Env.with_type_declaration env type_name constructor in
      Ok env)
;;
