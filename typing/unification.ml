open! Core
open! Import

let debug = false

let error ~annotations:(primary :: _ : Constraints.Annotations.t) message =
  let loc =
    match primary with
    | Expression_should_have_type (expr, _) -> Expression.loc expr
    | Pattern_should_have_type (pat, _) -> Pattern.loc pat
    | Var_requires_type_class (ident, _) -> ident.loc
  in
  Type_error.error_string ~loc message
;;

module Make (Lookup : sig
    type t

    val unify_var_var : t -> Type.Var.t -> Type.Var.t -> unit
    val unify_var_ty : t -> Type.Var.t -> Type.t -> unit
    val var : t -> Type.Var.t -> Type.t
  end) =
struct
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

  let rec normalize_ty t (ty : Type.t) ~env =
    let open Result.Let_syntax in
    match ty with
    | Var (v, _) ->
      (match Lookup.var t v with
       | Var _ as ty -> Ok ty
       | ty ->
         if debug then print_s [%message "normalising" (v : Type.Var.t) (ty : Type.t)];
         normalize_ty t ty ~env)
    | Apply ((name, args), annot) ->
      let%bind args = List.map args ~f:(normalize_ty t ~env) |> Result.all in
      Ok (Type.Apply ((name, args), annot))
    | Fun ((args, result), annot) ->
      let%bind args = List.map ~f:(normalize_ty t ~env) args |> Result.all in
      let%bind result = normalize_ty t result ~env in
      Ok (Type.Fun ((args, result), annot))
    | Tuple (types, annot) ->
      let%bind types = List.map types ~f:(normalize_ty t ~env) |> Result.all in
      Ok (Type.Tuple (types, annot))
  ;;

  let rec unify_ty_ty t ty1 ty2 ~env ~annotations =
    let open Result.Let_syntax in
    if debug then print_s [%message "unify_ty_ty" (ty1 : Type.t) (ty2 : Type.t)];
    let%bind ty1 = normalize_ty t ty1 ~env in
    let%bind ty2 = normalize_ty t ty2 ~env in
    (* FIXME: which one is the one we care about? *)
    let loc = Constraints.Annotations.primary_loc annotations in
    match ty1, ty2 with
    | Fun ((args_left, result_left), _), Fun ((args_right, result_right), _) ->
      let%bind () =
        iter2_result args_left args_right ~f:(fun arg_left arg_right ->
          unify_ty_ty t arg_left arg_right ~env ~annotations)
        |> Result.map_error ~f:(function
          | `Error err -> err
          | `Mismatched_lengths ->
            Type_error.of_string
              ~loc
              [%string
                "A different number of function arguments was provided to what we \
                 expected."])
      in
      unify_ty_ty t result_left result_right ~env ~annotations
    | Tuple (t1s, _), Tuple (t2s, _) ->
      iter2_result t1s t2s ~f:(fun ty1 ty2 -> unify_ty_ty t ty1 ty2 ~env ~annotations)
      |> Result.map_error ~f:(function
        | `Error err -> err
        | `Mismatched_lengths ->
          Type_error.of_string
            ~loc
            [%string "Tuple arguments had different number of members."])
    | Apply (((name1, _), args1), _), Apply (((name2, _), args2), _) ->
      let%bind () =
        (* FIXME: follow type aliases *)
        if Type_name.equal name1 name2
        then Ok ()
        else
          Type_error.error_string
            ~loc
            [%string "Types %{name1#Type_name} and %{name2#Type_name} are not equal."]
      in
      iter2_result args1 args2 ~f:(fun ty1 ty2 -> unify_ty_ty t ty1 ty2 ~env ~annotations)
      |> Result.map_error ~f:(function
        | `Error err -> err
        | `Mismatched_lengths ->
          Type_error.of_string
            ~loc
            [%string "Tuple arguments had different number of members."])
    | Var (v1, _), Var (v2, _) ->
      Lookup.unify_var_var t v1 v2;
      Ok ()
    | Var (v, _), ty | ty, Var (v, _) ->
      if Type.occurs ty ~var:v
      then
        Type_error.error_string
          ~loc
          [%string "Type variable %{v#Type.Var} occurs in another type."]
      else (
        Lookup.unify_var_ty t v ty;
        Ok ())
    | t1, t2 ->
      Type_error.error_s
        ~loc
        [%message "Failed to unify types" (t1 : Type.t) (t2 : Type.t)]
  ;;
end
