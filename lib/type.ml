open! Core
open! Import
module Var = Unique_id.Int ()
module Id = Unique_id.Int ()

type t =
  | Var of Var.t
  | Apply of Type_name.t * t list
  | Fun of t list * t
  | Tuple of t list
  | Intrinsic of Intrinsic.Type.t
[@@deriving sexp_of]

let const t = Apply (t, [])
let intrinsic i = Intrinsic i

let rec free_type_vars t =
  match t with
  | Var v -> Var.Set.singleton v
  | Apply (_, ts) ->
    List.fold ts ~init:Var.Set.empty ~f:(fun acc t -> Set.union acc (free_type_vars t))
  | Fun (args, r) ->
    Set.union
      (List.fold args ~init:Var.Set.empty ~f:(fun acc l ->
         Set.union acc (free_type_vars l)))
      (free_type_vars r)
  | Tuple ts ->
    List.fold ts ~init:Var.Set.empty ~f:(fun acc t -> Set.union acc (free_type_vars t))
  | Intrinsic _ -> Var.Set.empty
;;

let rec occurs t ~var =
  match t with
  | Var var' -> Var.equal var var'
  | Apply (_, ts) -> List.exists ts ~f:(fun t -> occurs t ~var)
  | Fun (args, res) -> List.exists args ~f:(occurs ~var) || occurs res ~var
  | Tuple ts -> List.exists ts ~f:(fun t -> occurs t ~var)
  | Intrinsic _ -> false
;;

let rec subst t ~replacements =
  match t with
  | Var v ->
    (match Map.find replacements v with
     | Some t -> t
     | None -> Var v)
  | Apply (name, ts) -> Apply (name, List.map ts ~f:(fun t -> subst t ~replacements))
  | Fun (args, r) -> Fun (List.map args ~f:(subst ~replacements), subst r ~replacements)
  | Tuple ts -> Tuple (List.map ts ~f:(subst ~replacements))
  | Intrinsic _ -> t
;;

let rec of_ast (t : Ast.Type.t) ~var_mapping =
  match t with
  | Var v -> Var (Map.find_exn var_mapping v)
  | Intrinsic i -> Intrinsic i
  | Apply (name, ts) -> Apply (name, List.map ts ~f:(of_ast ~var_mapping))
  | Fun (args, r) -> Fun (List.map args ~f:(of_ast ~var_mapping), of_ast r ~var_mapping)
  | Tuple ts -> Tuple (List.map ts ~f:(of_ast ~var_mapping))
;;

module Poly = struct
  type ty = t [@@deriving sexp_of]

  type t =
    { quantifiers : Var.Set.t
    ; ty : ty
    }
  [@@deriving sexp_of]

  let ty_subst = subst

  let subst t ~(replacements : ty Var.Map.t) =
    assert (
      not (Map.existsi replacements ~f:(fun ~key ~data:_ -> Set.mem t.quantifiers key)));
    { t with ty = subst t.ty ~replacements }
  ;;

  let init t =
    let fresh_vars = Set.to_map t.quantifiers ~f:(fun _ -> Var (Var.create ())) in
    ty_subst t.ty ~replacements:fresh_vars
  ;;

  let free_type_vars t = Set.diff (free_type_vars t.ty) t.quantifiers

  let env_free_type_vars (env : t Ident.Map.t) =
    Map.fold env ~init:Var.Set.empty ~f:(fun ~key:_ ~data:pty acc ->
      Set.union acc (free_type_vars pty))
  ;;

  let ty_of_ast = of_ast

  let of_ast (t : Ast.Type.Poly.t) ~var_mapping =
    let%tydi { quantifiers; ty } = t in
    let new_var_mappings = List.map quantifiers ~f:(fun v -> v, Var.create ()) in
    let quantifiers = List.map new_var_mappings ~f:snd in
    let var_mapping =
      List.fold new_var_mappings ~init:var_mapping ~f:(fun acc (v, var) ->
        Map.set acc ~key:v ~data:var)
    in
    { quantifiers = Var.Set.of_list quantifiers; ty = ty_of_ast ty ~var_mapping }
  ;;
end

let generalize (typ : t) ~(env : Poly.t Ident.Map.t) : Poly.t =
  let free_vars = Set.diff (free_type_vars typ) (Poly.env_free_type_vars env) in
  { quantifiers = free_vars; ty = typ }
;;

module Constructor = struct
  type ty = t [@@deriving sexp_of]

  module Shape = struct
    type t =
      | Alias of ty
      | Record of
          { fields : (Field_name.t * ty) list
          ; id : Id.t
          }
      | Variant of
          { constructors : (Constructor.t * ty option) list
          ; id : Id.t
          }
    [@@deriving sexp_of]
  end

  type t =
    { shape : Shape.t
    ; args : Var.t list
    }
  [@@deriving sexp_of]
end

let rec type_names_are_equivalent n1 n2 (tyenv : Constructor.t Type_name.Map.t) =
  match Map.find tyenv n1, Map.find tyenv n2 with
  | Some t1, Some t2 ->
    (match t1.shape, t2.shape with
     | Alias t1, Alias t2 -> types_are_equivalent t1 t2 tyenv
     | Record { id = id1; _ }, Record { id = id2; _ } -> Id.equal id1 id2
     | Variant { id = id1; _ }, Variant { id = id2; _ } -> Id.equal id1 id2
     | _ -> false)
  | None, Some _ -> failwith [%string "Type not found in tyenv %{n1#Type_name}"]
  | Some _, None -> failwith [%string "Type not found in tyenv %{n2#Type_name}"]
  | None, None ->
    failwith [%string "Types not found in tyenv %{n1#Type_name} and %{n2#Type_name}"]

and types_are_equivalent t1 t2 tyenv =
  match t1, t2 with
  | Var v1, Var v2 -> Var.equal v1 v2
  | Apply (n1, l1), Apply (n2, l2) ->
    type_names_are_equivalent n1 n2 tyenv
    && List.for_all2_exn l1 l2 ~f:(fun t1 t2 -> types_are_equivalent t1 t2 tyenv)
  | Fun (l1, r1), Fun (l2, r2) ->
    List.for_all2_exn l1 l2 ~f:(fun t1 t2 -> types_are_equivalent t1 t2 tyenv)
    && types_are_equivalent r1 r2 tyenv
  | Tuple l1, Tuple l2 ->
    List.for_all2_exn l1 l2 ~f:(fun t1 t2 -> types_are_equivalent t1 t2 tyenv)
  | Intrinsic i1, Intrinsic i2 -> Intrinsic.Type.equal i1 i2
  | Apply (n, _), other | other, Apply (n, _) ->
    (match Map.find tyenv n with
     | Some { args = _; shape = Alias t } -> types_are_equivalent t other tyenv
     | None -> failwith [%string "Type not found in tyenv %{n#Type_name}"]
     | _ -> false)
  | _ -> false
;;

let rec unify' t1 t2 ~tyenv ~acc : t Var.Map.t =
  match t1, t2 with
  | Apply (n1, l1), Apply (n2, l2) ->
    if type_names_are_equivalent n1 n2 tyenv
    then List.fold2_exn l1 l2 ~init:acc ~f:(fun acc t1 t2 -> unify' t1 t2 ~tyenv ~acc)
    else failwith [%string "Type Mismatch: %{n1#Type_name} != %{n2#Type_name}"]
  | Var v, t | t, Var v ->
    if occurs t ~var:v then failwith "occur check failed" else Map.set acc ~key:v ~data:t
  | Fun (l1, r1), Fun (l2, r2) ->
    let acc =
      List.fold2_exn l1 l2 ~init:acc ~f:(fun acc t1 t2 -> unify' t1 t2 ~tyenv ~acc)
    in
    let r1 = subst r1 ~replacements:acc in
    let r2 = subst r2 ~replacements:acc in
    unify' r1 r2 ~tyenv ~acc
  | Tuple l1, Tuple l2 ->
    List.fold2_exn l1 l2 ~init:acc ~f:(fun acc t1 t2 -> unify' t1 t2 ~tyenv ~acc)
  | Intrinsic i1, Intrinsic i2 ->
    if Intrinsic.Type.equal i1 i2
    then acc
    else failwith [%string "Type Mismatch %{i1#Intrinsic.Type} != %{i2#Intrinsic.Type}"]
  | Apply (n, _), other | other, Apply (n, _) ->
    (match Map.find tyenv n with
     | Some { args = _; shape = Alias t } -> unify' t other ~tyenv ~acc
     | None -> failwith [%string "Type not found in tyenv %{n#Type_name}"]
     | _ -> raise_s [%message "Type Mismatch" (t1 : t) (t2 : t)])
  | _ -> raise_s [%message "Type Mismatch" (t1 : t) (t2 : t)]
;;

let unify t1 t2 ~tyenv = unify' t1 t2 ~tyenv ~acc:Var.Map.empty
