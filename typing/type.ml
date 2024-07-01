open! Core
open! Import
module Var = Unique_id.Int ()
module Id = Unique_id.Int ()

module A = struct
  type apply = unit [@@deriving sexp_of]
  type fun_ = unit [@@deriving sexp_of]
  type tuple = unit [@@deriving sexp_of]

  type type_constructor =
    [ `Built_in
    | `Position of Span.t
    ]
  [@@deriving sexp_of]

  type var = unit [@@deriving sexp_of]
end

include Base_ast.Type.Make (Var) (A)

let var x = Var (x, ())

let exists_free_type_var t ~f =
  let exception Found in
  try
    iter_free_type_vars t ~f:(fun v -> if f v then raise_notrace Found);
    false
  with
  | Found -> true
;;

let const t = Apply ((t, []), ())

let intrinsic ?(loc = `Built_in) name =
  Apply (((Intrinsic.Type.type_name name, loc), []), ())
;;

let free_type_vars t =
  fold_free_type_vars t ~init:Var.Set.empty ~f:(fun acc v -> Set.add acc v)
;;

let occurs t ~var = exists_free_type_var t ~f:(Var.equal var)

let rec subst t ~replacements =
  match t with
  | Var (v, annot) ->
    (match Map.find replacements v with
     | Some t -> t
     | None -> Var (v, annot))
  | Apply ((name, ts), annot) ->
    Apply ((name, List.map ts ~f:(fun t -> subst t ~replacements)), annot)
  | Fun ((args, r), annot) ->
    Fun ((List.map args ~f:(subst ~replacements), subst r ~replacements), annot)
  | Tuple (ts, annot) -> Tuple (List.map ts ~f:(subst ~replacements), annot)
;;

let rec of_ast (t : Ast.Type.t) ~var_mapping =
  match t with
  | Var (v, _) -> Var (Map.find_exn var_mapping v, ())
  | Apply (((name, name_pos), ts), _) ->
    Apply (((name, `Position name_pos), List.map ts ~f:(of_ast ~var_mapping)), ())
  | Fun ((args, r), _) ->
    Fun ((List.map args ~f:(of_ast ~var_mapping), of_ast r ~var_mapping), ())
  | Tuple (ts, _) -> Tuple (List.map ts ~f:(of_ast ~var_mapping), ())
;;

module Poly = struct
  type ty = t

  include Poly

  let ty_subst = subst
  let mono body = { body; quantifiers = []; constraints = [] }

  let subst t ~(replacements : ty Var.Map.t) =
    assert (not (List.exists t.quantifiers ~f:(fun v -> Map.mem replacements v)));
    { t with body = subst t.body ~replacements }
  ;;

  let init t =
    let fresh_vars' =
      List.map t.quantifiers ~f:(fun q -> q, Var.create ()) |> Var.Map.of_alist_exn
    in
    let fresh_vars = Map.map fresh_vars' ~f:(fun var -> Var (var, ())) in
    let constraints =
      List.map t.constraints ~f:(fun constraints_ ->
        { constraints_ with arg = ty_subst ~replacements:fresh_vars constraints_.arg })
    in
    ty_subst t.body ~replacements:fresh_vars, constraints
  ;;

  let free_type_vars t = Set.diff (free_type_vars t.body) (Var.Set.of_list t.quantifiers)

  let env_free_type_vars (env : t Ident.Map.t) =
    Map.fold env ~init:Var.Set.empty ~f:(fun ~key:_ ~data:pty acc ->
      Set.union acc (free_type_vars pty))
  ;;

  let ty_of_ast = of_ast

  let of_ast (t : Ast.Type.Poly.t) ~var_mapping =
    let%tydi { quantifiers; body; constraints } = t in
    let new_var_mappings = List.map quantifiers ~f:(fun v -> v, Var.create ()) in
    let quantifiers = List.map new_var_mappings ~f:snd in
    let var_mapping =
      List.fold new_var_mappings ~init:var_mapping ~f:(fun acc (v, var) ->
        Map.set acc ~key:v ~data:var)
    in
    { quantifiers
    ; body = ty_of_ast body ~var_mapping
    ; constraints =
        List.map constraints ~f:(fun { type_class; arg } : Constraint.t ->
          { type_class; arg = ty_of_ast arg ~var_mapping })
    }
  ;;
end

let generalize (typ : t) ~(env : Poly.t Ident.Map.t) : Poly.t =
  let free_vars = Set.diff (free_type_vars typ) (Poly.env_free_type_vars env) in
  { quantifiers = Set.to_list free_vars; body = typ; constraints = [] }
;;

module Constructor = struct
  type ty = t [@@deriving sexp_of]

  module Shape = struct
    type t =
      | Intrinsic of Intrinsic.Type.t
      | Record of
          { fields : ((Field_name.t * Span.t) * ty) list
          ; id : Id.t
          }
      | Variant of
          { constructors : ((Constructor.t * Span.t) * ty option) list
          ; id : Id.t
          }
    [@@deriving sexp_of]
  end

  type t =
    { shape : Shape.t
    ; args : Var.t list
    ; loc : [ `Built_in | `Position of Span.t ]
    }
  [@@deriving sexp_of]
end
