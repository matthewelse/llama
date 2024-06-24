open! Core
open! Import

let debug = false

type t = { vars : Type.t Union_find.t Type.Var.Table.t }

let create () = { vars = Type.Var.Table.create () }

let sexp_of_t t =
  let vars =
    Hashtbl.to_alist t.vars
    |> List.map ~f:(fun (var, ty) ->
      let ty = Union_find.get ty in
      var, ty)
  in
  [%sexp { vars : (Type.Var.t * Type.t) list }]
;;

let rec normalize_ty t (ty : Type.t) ~env =
  let open Result.Let_syntax in
  match ty with
  | Intrinsic _ -> Ok ty
  | Var v ->
    (match Hashtbl.find t.vars v with
     | None -> Ok ty
     | Some uf ->
       (match Union_find.get uf with
        | Var v -> Ok (Var v)
        | ty ->
          if debug then print_s [%message "normalising" (v : Type.Var.t) (ty : Type.t)];
          normalize_ty t ty ~env))
  | Apply (name, args) ->
    let%bind args = List.map args ~f:(normalize_ty t ~env) |> Result.all in
    (match%bind Env.type_declaration env name.value ~loc:name.loc with
     | { shape = Alias (Intrinsic _); args = _ } ->
       (* FIXME melse: something about this abstraction feels wrong - maybe I should just
          remove primitive from [Type.t]? *)
       (* Treat intrinsic type aliases as opaque. *)
       Ok (Type.Apply (name, args))
     | { shape = Alias ty; args = decl_args } ->
       let replacements =
         List.map2_exn decl_args args ~f:(fun decl_arg arg -> decl_arg, arg)
         |> Type.Var.Map.of_alist_exn
       in
       normalize_ty t (Type.subst ty ~replacements) ~env
     | _ -> Ok (Type.Apply (name, args)))
  | Fun (args, result) ->
    let%bind args = List.map ~f:(normalize_ty t ~env) args |> Result.all in
    let%bind result = normalize_ty t result ~env in
    Ok (Type.Fun (args, result))
  | Tuple types ->
    let%bind types = List.map types ~f:(normalize_ty t ~env) |> Result.all in
    Ok (Type.Tuple types)
;;

let rec iter_result xs ~f =
  match xs with
  | [] -> Ok ()
  | x :: xs ->
    let%bind.Result () = f x in
    iter_result xs ~f
;;

let rec iter2_result xs ys ~f =
  match xs, ys with
  | [], [] -> Ok ()
  | x :: xs, y :: ys ->
    let%bind.Result () = f x y |> Result.map_error ~f:(fun err -> `Error err) in
    iter2_result xs ys ~f
  | [], _ :: _ | _ :: _, [] -> Error `Mismatched_lengths
;;

let lookup_var t v =
  Hashtbl.find_or_add t.vars v ~default:(fun () -> Union_find.create (Type.Var v))
;;

let unify_var_var t v1 v2 =
  let v1' = lookup_var t v1 in
  let v2' = lookup_var t v2 in
  assert (Type.is_var (Union_find.get v1'));
  assert (Type.is_var (Union_find.get v2'));
  Union_find.union v1' v2'
;;

let rec unify_ty_ty t ty1 ty2 ~env =
  let open Result.Let_syntax in
  let loc =
    (* FIXME: todo *)
    Span.dummy
  in
  if debug then print_s [%message "unify_ty_ty" (ty1 : Type.t) (ty2 : Type.t)];
  let%bind ty1 = normalize_ty t ty1 ~env in
  let%bind ty2 = normalize_ty t ty2 ~env in
  match ty1, ty2 with
  | Intrinsic i1, Intrinsic i2 ->
    if Intrinsic.Type.equal i1 i2
    then Ok ()
    else
      Type_error.error_string
        ~loc:Span.dummy
        [%string "Incompatible types: %{i1#Intrinsic.Type} and %{i2#Intrinsic.Type}"]
  | Fun (args_left, result_left), Fun (args_right, result_right) ->
    let%bind () =
      iter2_result args_left args_right ~f:(fun arg_left arg_right ->
        unify_ty_ty t arg_left arg_right ~env)
      |> Result.map_error ~f:(function
        | `Error err -> err
        | `Mismatched_lengths ->
          Type_error.of_string
            ~loc
            [%string
              "A different number of function arguments was provided to what we expected."])
    in
    unify_ty_ty t result_left result_right ~env
  | Tuple t1s, Tuple t2s ->
    iter2_result t1s t2s ~f:(fun ty1 ty2 -> unify_ty_ty t ty1 ty2 ~env)
    |> Result.map_error ~f:(function
      | `Error err -> err
      | `Mismatched_lengths ->
        Type_error.of_string
          ~loc
          [%string "Tuple arguments had different number of members."])
  | Apply (name1, args1), Apply (name2, args2) ->
    let%bind () =
      (* FIXME: follow type aliases *)
      if Type_name.equal name1.value name2.value
      then Ok ()
      else
        Type_error.error_string
          ~loc:(* FIXME: which one is the one we care about? *)
               name1.loc
          [%string
            "Types %{name1.value#Type_name} and %{name2.value#Type_name} are not equal."]
    in
    iter2_result args1 args2 ~f:(fun ty1 ty2 -> unify_ty_ty t ty1 ty2 ~env)
    |> Result.map_error ~f:(function
      | `Error err -> err
      | `Mismatched_lengths ->
        Type_error.of_string
          ~loc
          [%string "Tuple arguments had different number of members."])
  | Var v1, Var v2 ->
    unify_var_var t v1 v2;
    Ok ()
  | Var v, ty | ty, Var v ->
    if Type.occurs ty ~var:v
    then
      Type_error.error_string
        ~loc
        [%string "Type variable %{v#Type.Var} occurs in another type."]
    else (
      let repr = lookup_var t v in
      if debug then print_s [%message (v : Type.Var.t) ~equals:(ty : Type.t)];
      Union_find.set repr ty;
      Ok ())
  | Apply (type_name, _), Intrinsic intrinsic | Intrinsic intrinsic, Apply (type_name, _)
    ->
    let%bind decl = Env.type_declaration env type_name.value ~loc:type_name.loc in
    let%bind intrinsic' =
      match decl.shape with
      | Alias (Intrinsic i) -> Ok i
      | _ ->
        Type_error.error_string
          ~loc:type_name.loc
          [%string
            "Failed to unify types (got %{type_name.value#Type_name}, expected \
             %{intrinsic#Intrinsic.Type})"]
    in
    if Intrinsic.Type.equal intrinsic intrinsic'
    then Ok ()
    else
      Type_error.error_string
        ~loc:type_name.loc
        "Failed to unify types (mismatching intrinsic types)"
  | t1, t2 ->
    Type_error.error_s ~loc [%message "Failed to unify types" (t1 : Type.t) (t2 : Type.t)]
;;

let solve t (constraints : Constraints.t) ~(env : Env.t) =
  let open Result.Let_syntax in
  let%bind () =
    iter_result (Constraints.to_list constraints) ~f:(function
      | Same_type (t1, t2, _note) -> unify_ty_ty t t1 t2 ~env)
  in
  Ok
    { env with
      values =
        Map.map env.values ~f:(fun typ ->
          let ty = normalize_ty t typ.ty ~env |> Type_error.ok_exn in
          if debug
          then print_s [%message "final normalization" (typ : Type.Poly.t) (ty : Type.t)];
          { typ with ty })
    }
;;
