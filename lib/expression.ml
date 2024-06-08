open! Core

module Const = struct
  type t =
    | Int of int
    | String of string
  [@@deriving sexp_of]

  let type_name t =
    match t with
    | Int _ -> Type.Name.Built_in.int
    | String _ -> Type.Name.Built_in.string
  ;;

  let type_of t : Type.t = Apply (type_name t, [])
end

type t =
  | Var of Ident.t
  | Apply of t * t
  | Lambda of Ident.t * t
  | Let of
      { name : Ident.t
      ; value : t
      ; body : t
      }
  | Const of Const.t
  | Tuple of t list
[@@deriving sexp_of]

let subst_env (env : Type.Poly.t Ident.Map.t) ~replacements =
  Map.map env ~f:(Type.Poly.subst ~replacements)
;;

let rec type_of t (env : Type.Poly.t Ident.Map.t)
  : (Type.t Type.Var.Map.t * Type.t) Or_error.t
  =
  let open Or_error.Let_syntax in
  match t with
  | Var name ->
    let%bind poly_type =
      Map.find env name
      |> Or_error.of_option
           ~error:(Error.of_string [%string "Unbound variable [%{name#Ident}]"])
    in
    let type_ = Type.Poly.init poly_type in
    Ok (Type.Var.Map.empty, type_)
  | Apply (l, r) ->
    let%bind s1, type_l = type_of l env in
    let env = subst_env env ~replacements:s1 in
    let%bind s2, type_r = type_of r env in
    let s = Map.merge_skewed s1 s2 ~combine:(fun ~key:_ _ t2 -> t2) in
    let type_l = Type.subst type_l ~replacements:s in
    (* Unify *)
    let result_var = Type.Var (Type.Var.create ()) in
    let s = Type.unify' type_l (Fun (type_r, result_var)) ~acc:s in
    Ok (s, Type.subst result_var ~replacements:s)
  | Lambda (name, body) ->
    let name_var = Type.Var.create () in
    let%bind s, body_type =
      type_of
        body
        (Map.set
           env
           ~key:name
           ~data:{ ty = Var name_var; quantifiers = Type.Var.Set.empty })
    in
    let body_type = Type.subst body_type ~replacements:s in
    Ok (s, Type.Fun (Var name_var, body_type))
  | Let { name; value; body } ->
    let%bind s1, t1 = type_of value env in
    let generalized_type = Type.generalize t1 ~env in
    let env = Map.set (subst_env env ~replacements:s1) ~key:name ~data:generalized_type in
    type_of body env
  | Const c -> Ok (Type.Var.Map.empty, Const.type_of c)
  | Tuple ts ->
    let%bind replacements, types =
      List.fold_result
        ts
        ~init:(Type.Var.Map.empty, [])
        ~f:(fun (replacements, types) t ->
          let%bind replacements', ty = type_of t env in
          Ok
            ( Map.merge_skewed replacements replacements' ~combine:(fun ~key:_ _ t2 -> t2)
            , ty :: types ))
    in
    Ok (replacements, Type.Tuple types)
;;

let type_of t =
  let%bind.Or_error type_env, ty = type_of t Ident.Map.empty in
  let type_vars =
    Map.filter type_env ~f:(function
      | Var _ -> false
      | _ -> true)
    |> Map.key_set
  in
  let free_vars_in_type = Type.free_type_vars ty in
  let unsubstituted_vars = Set.inter free_vars_in_type type_vars in
  (* If there are vars in the type that haven't been substituted, return an error *)
  if Set.is_empty unsubstituted_vars
  then Ok ty
  else failwith "BUG! Unsubstituted type vars in type"
;;

let type_of_let_binding t =
  let%bind.Or_error ty = type_of t in
  Ok (Type.generalize ty ~env:Ident.Map.empty)
;;
