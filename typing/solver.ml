open! Core
open! Import

type t = { vars : Type.t Union_find.t Type.Var.Table.t }

let create () = { vars = Type.Var.Table.create () }

let rec normalize_ty t (ty : Type.t) =
  match ty with
  | Intrinsic _ -> ty
  | Var v ->
    (match Hashtbl.find t.vars v with
     | None -> ty
     | Some uf -> Union_find.get uf)
  | Apply (name, args) -> Apply (name, List.map args ~f:(normalize_ty t))
  | Fun (args, result) -> Fun (List.map ~f:(normalize_ty t) args, normalize_ty t result)
  | Tuple types -> Tuple (List.map types ~f:(normalize_ty t))
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

let lookup_var t v = Hashtbl.find t.vars v

let unify_var_var t v1 v2 =
  match lookup_var t v1, lookup_var t v2 with
  | None, None ->
    let v1' = Union_find.create (Type.Var v1) in
    let v2' = Union_find.create (Type.Var v2) in
    Union_find.union v1' v2';
    Hashtbl.set t.vars ~key:v1 ~data:v1';
    Hashtbl.set t.vars ~key:v2 ~data:v2';
    Ok ()
  | Some v1', None ->
    let v2' = Union_find.create (Type.Var v2) in
    Union_find.union v1' v2';
    Hashtbl.set t.vars ~key:v2 ~data:v2';
    Ok ()
  | None, Some v2' ->
    let v1' = Union_find.create (Type.Var v1) in
    Union_find.union v1' v2';
    Hashtbl.set t.vars ~key:v1 ~data:v1';
    Ok ()
  | Some v1', Some v2' ->
    if Union_find.same_class v1' v2'
    then Ok ()
    else
      Or_error.error_string [%string "Types not equal: %{v1#Type.Var} and %{v2#Type.Var}"]
;;

let rec unify_ty_ty t ty1 ty2 =
  let open Result.Let_syntax in
  let ty1 = normalize_ty t ty1 in
  let ty2 = normalize_ty t ty2 in
  match ty1, ty2 with
  | Intrinsic i1, Intrinsic i2 ->
    if Intrinsic.Type.equal i1 i2
    then Ok ()
    else
      Or_error.error_string
        [%string "Incompatible types: %{i1#Intrinsic.Type} and %{i2#Intrinsic.Type}"]
  | Fun (args_left, result_left), Fun (args_right, result_right) ->
    let%bind () =
      iter2_result args_left args_right ~f:(fun arg_left arg_right ->
        unify_ty_ty t arg_left arg_right)
      |> Result.map_error ~f:(function
        | `Error err -> err
        | `Mismatched_lengths ->
          Error.of_string
            [%string
              "A different number of function arguments was provided to what we expected."])
    in
    unify_ty_ty t result_left result_right
  | Tuple t1s, Tuple t2s ->
    iter2_result t1s t2s ~f:(fun ty1 ty2 -> unify_ty_ty t ty1 ty2)
    |> Result.map_error ~f:(function
      | `Error err -> err
      | `Mismatched_lengths ->
        Error.of_string [%string "Tuple arguments had different number of members."])
  | Apply (name1, args1), Apply (name2, args2) ->
    let%bind () =
      (* FIXME: follow type aliases *)
      if Type_name.equal name1 name2
      then Ok ()
      else
        Or_error.error_string
          [%string "Types %{name1#Type_name} and %{name2#Type_name} were not equal."]
    in
    iter2_result args1 args2 ~f:(fun ty1 ty2 -> unify_ty_ty t ty1 ty2)
    |> Result.map_error ~f:(function
      | `Error err -> err
      | `Mismatched_lengths ->
        Error.of_string [%string "Tuple arguments had different number of members."])
  | Var v1, Var v2 -> unify_var_var t v1 v2
  | Var v, ty | ty, Var v ->
    if Type.occurs ty ~var:v
    then
      Or_error.error_string
        [%string "Type variable %{v#Type.Var} occurs in another type."]
    else (
      let repr = Hashtbl.find_or_add t.vars v ~default:(fun () -> Union_find.create ty) in
      Union_find.set repr ty;
      Ok ())
  | t1, t2 ->
    Or_error.error_s [%message "Failed to unify types" (t1 : Type.t) (t2 : Type.t)]
;;

let solve (constraints : Constraints.t) ~(env : Env.t) =
  let open Result.Let_syntax in
  let t = create () in
  let%bind () =
    iter_result (Constraints.to_list constraints) ~f:(function
      | Same_type (t1, t2, _note) -> unify_ty_ty t t1 t2)
  in
  Ok
    { env with
      values = Map.map env.values ~f:(fun typ -> { typ with ty = normalize_ty t typ.ty })
    }
;;
