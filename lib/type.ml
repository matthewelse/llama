open! Core

module Name = struct
  include
    String_id.Make
      (struct
        let module_name = "Type.Name"
      end)
      ()

  module Built_in = struct
    let int = of_string "%int"
    let bool = of_string "%bool"
    let unit = of_string "%unit"
    let string = of_string "%string"
  end
end

module Var = Unique_id.Int ()

type t =
  | Var of Var.t
  | Apply of Name.t * t list
  | Fun of t * t
  | Tuple of t list
[@@deriving sexp_of]

let rec free_type_vars t =
  match t with
  | Var v -> Var.Set.singleton v
  | Apply (_, ts) ->
    List.fold ts ~init:Var.Set.empty ~f:(fun acc t -> Set.union acc (free_type_vars t))
  | Fun (l, r) -> Set.union (free_type_vars l) (free_type_vars r)
  | Tuple ts ->
    List.fold ts ~init:Var.Set.empty ~f:(fun acc t -> Set.union acc (free_type_vars t))
;;

let rec occurs t ~var =
  match t with
  | Var var' -> Var.equal var var'
  | Apply (_, ts) -> List.exists ts ~f:(fun t -> occurs t ~var)
  | Fun (t1, t2) -> occurs t1 ~var || occurs t2 ~var
  | Tuple ts -> List.exists ts ~f:(fun t -> occurs t ~var)
;;

let rec subst t ~replacements =
  match t with
  | Var v ->
    (match Map.find replacements v with
     | Some t -> t
     | None -> Var v)
  | Apply (name, ts) -> Apply (name, List.map ts ~f:(fun t -> subst t ~replacements))
  | Fun (l, r) -> Fun (subst l ~replacements, subst r ~replacements)
  | Tuple ts -> Tuple (List.map ts ~f:(subst ~replacements))
;;

let rec unify' t1 t2 ~acc : t Var.Map.t =
  match t1, t2 with
  | Apply (n1, l1), Apply (n2, l2) when Name.equal n1 n2 ->
    List.fold2_exn l1 l2 ~init:acc ~f:(fun acc t1 t2 -> unify' t1 t2 ~acc)
  | Var v, t | t, Var v ->
    if occurs t ~var:v then failwith "occur check failed" else Map.set acc ~key:v ~data:t
  | Fun (l1, r1), Fun (l2, r2) ->
    let acc = unify' l1 l2 ~acc in
    let r1 = subst r1 ~replacements:acc in
    let r2 = subst r2 ~replacements:acc in
    unify' r1 r2 ~acc
  | Tuple l1, Tuple l2 ->
    List.fold2_exn l1 l2 ~init:acc ~f:(fun acc t1 t2 -> unify' t1 t2 ~acc)
  | _ -> raise_s [%message "Type Mismatch" (t1 : t) (t2 : t)]
;;

let unify t1 t2 = unify' t1 t2 ~acc:Var.Map.empty

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
end

let generalize (typ : t) ~(env : Poly.t Ident.Map.t) : Poly.t =
  let free_vars = Set.diff (free_type_vars typ) (Poly.env_free_type_vars env) in
  { quantifiers = free_vars; ty = typ }
;;
