open! Core
open! Import

module Constraint = struct
  type t =
    | Same_type of Type.t * Type.t * Annotation.t Nonempty_list.t
    | Implements_type_class of Type_class_name.t * Type.t * Annotation.t Nonempty_list.t
  [@@deriving sexp_of]
end

let debug = false

module T = struct
  type t =
    { vars : Type.t Union_find.t Type_var.Table.t
    ; constraints : Type.t list Type_class_name.Table.t
    }

  let lookup_var t v =
    Hashtbl.find_or_add t.vars v ~default:(fun () -> Union_find.create (Type.Var (v, ())))
  ;;

  let unify_var_var t v1 v2 =
    let v1' = lookup_var t v1 in
    let v2' = lookup_var t v2 in
    assert (Type.is_var (Union_find.get v1'));
    assert (Type.is_var (Union_find.get v2'));
    Union_find.union v1' v2'
  ;;

  let unify_var_ty t v ty =
    let repr = lookup_var t v in
    if debug then print_s [%message (v : Type_var.t) ~equals:(ty : Type.t)];
    Union_find.set repr ty
  ;;

  let var t var = lookup_var t var |> Union_find.get
end

include T
module Unify = Unification.Make (T)

let normalize_ty = Unify.normalize_ty

let create () =
  { vars = Type_var.Table.create (); constraints = Type_class_name.Table.create () }
;;

let rec iter_result xs ~f =
  match xs with
  | [] -> Ok ()
  | x :: xs ->
    let%bind.Result () = f x in
    iter_result xs ~f
;;

let sexp_of_t t =
  let vars =
    Hashtbl.to_alist t.vars
    |> List.map ~f:(fun (var, ty) ->
      let ty = Union_find.get ty in
      var, ty)
  in
  [%sexp { vars : (Type_var.t * Type.t) list }]
;;

let solve t (constraints : Constraint.t list) ~(env : Env.t) =
  let open Result.Let_syntax in
  let%bind () =
    iter_result constraints ~f:(function
      | Same_type (t1, t2, annotations) -> Unify.unify_ty_ty t t1 t2 ~env ~annotations
      | Implements_type_class (type_class, arg, _annotations) ->
        Hashtbl.add_multi t.constraints ~key:type_class ~data:arg;
        Ok ())
  in
  Ok
    { env with
      values =
        Map.map env.values ~f:(fun typ ->
          let ty = normalize_ty t typ.body ~env |> Type_error.ok_exn in
          if debug
          then print_s [%message "final normalization" (typ : Type.Poly.t) (ty : Type.t)];
          { typ with body = ty })
    }
;;

let constraints t ~env =
  Hashtbl.to_alist t.constraints
  |> List.sort ~compare:[%compare: Type_class_name.t * _]
  |> List.map ~f:(fun (name, constraints) ->
    let%bind.Result constraints =
      List.map constraints ~f:(fun typ -> normalize_ty t ~env typ) |> Result.all
    in
    Ok (name, constraints))
  |> Result.all
;;
