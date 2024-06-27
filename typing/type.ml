open! Core
open! Import
module Var = Unique_id.Int ()
module Id = Unique_id.Int ()

type 'var t_generic =
  | Var of 'var
  | Apply of Type_name.t Located.t * 'var t_generic list
  | Fun of 'var t_generic list * 'var t_generic
  | Tuple of 'var t_generic list
  | Intrinsic of Intrinsic.Type.t
[@@deriving fold, iter, sexp_of, variants]

type t = Var.t t_generic [@@deriving sexp_of]

let fold_free_type_vars t ~init ~f = fold_t_generic f init t
let iter_free_type_vars t ~f = iter_t_generic f t

let exists_free_type_var t ~f =
  let exception Found in
  try
    iter_free_type_vars t ~f:(fun v -> if f v then raise_notrace Found);
    false
  with
  | Found -> true
;;

let const t = Apply (t, [])
let intrinsic i = Intrinsic i

let free_type_vars t =
  fold_free_type_vars t ~init:Var.Set.empty ~f:(fun acc v -> Set.add acc v)
;;

let occurs t ~var = exists_free_type_var t ~f:(Var.equal var)

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
  match t.desc with
  | Var v -> Var (Map.find_exn var_mapping v)
  | Apply (name, ts) -> Apply (name, List.map ts ~f:(of_ast ~var_mapping))
  | Fun (args, r) -> Fun (List.map args ~f:(of_ast ~var_mapping), of_ast r ~var_mapping)
  | Tuple ts -> Tuple (List.map ts.value ~f:(of_ast ~var_mapping))
  | Intrinsic i -> Intrinsic i
;;

module Constraint = struct
  type t =
    { type_class : Type_class_name.t
    ; args : Var.t list
    }
  [@@deriving sexp_of]
end

module Poly = struct
  type ty = t [@@deriving sexp_of]

  type t =
    { quantifiers : Var.Set.t
    ; ty : ty
    ; constraints : Constraint.t list
    }
  [@@deriving sexp_of]

  let ty_subst = subst
  let mono ty = { ty; quantifiers = Var.Set.empty; constraints = [] }

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
    { quantifiers = Var.Set.of_list quantifiers
    ; ty = ty_of_ast ty ~var_mapping
    ; constraints = []
    }
  ;;
end

let generalize (typ : t) ~(env : Poly.t Ident.Map.t) : Poly.t =
  let free_vars = Set.diff (free_type_vars typ) (Poly.env_free_type_vars env) in
  { quantifiers = free_vars; ty = typ; constraints = [] }
;;

module Constructor = struct
  type ty = t [@@deriving sexp_of]

  module Shape = struct
    type t =
      | Alias of ty
      | Record of
          { fields : (Field_name.t Located.t * ty) list
          ; id : Id.t
          }
      | Variant of
          { constructors : (Constructor.t Located.t * ty option) list
          ; id : Id.t
          }
    [@@deriving sexp_of]
  end

  type t =
    { shape : Shape.t
    ; args : Var.t list
    ; loc : Span.t
    }
  [@@deriving sexp_of]
end
