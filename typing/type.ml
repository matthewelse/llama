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

let pp_tv' formatter i =
  let chars = String.to_array "abcdefghijklmnopqrstuvwxyz" in
  let rec aux i =
    if i < 26
    then Format.pp_print_char formatter chars.(i)
    else (
      let c = chars.(i mod 26) in
      aux ((i / 26) - 1);
      Format.pp_print_char formatter c)
  in
  Format.pp_print_char formatter '\'';
  aux i
;;

let pp_tv formatter tv ~tvs =
  let i = Map.find_exn tvs tv in
  pp_tv' formatter i
;;

let rec pp_type' formatter ty ~tvs =
  match ty with
  | Var (tv, _) -> pp_tv formatter tv ~tvs
  | Apply (((n, _), []), _) -> Format.pp_print_string formatter (Type_name.to_string n)
  | Apply (((n, _), [ var ]), _) ->
    pp_type' ~tvs formatter var;
    Format.pp_print_string formatter " ";
    Format.pp_print_string formatter (Type_name.to_string n)
  | Apply (((n, _), vars), _) ->
    Format.pp_print_char formatter '(';
    Format.pp_print_list
      ~pp_sep:(fun formatter () -> Format.pp_print_string formatter ", ")
      (pp_type' ~tvs)
      formatter
      vars;
    Format.pp_print_string formatter ") ";
    Format.pp_print_string formatter (Type_name.to_string n)
  | Fun ((args, r), _) ->
    Format.pp_print_string formatter "(";
    Format.pp_print_list
      ~pp_sep:(fun formatter () -> Format.pp_print_string formatter ", ")
      (pp_type' ~tvs)
      formatter
      args;
    Format.pp_print_string formatter ")";
    Format.pp_print_string formatter " -> ";
    pp_type' formatter r ~tvs
  | Tuple (ts, _) ->
    Format.pp_print_char formatter '(';
    Format.pp_print_list
      ~pp_sep:(fun formatter () -> Format.pp_print_string formatter ", ")
      (pp_type' ~tvs)
      formatter
      ts;
    Format.pp_print_char formatter ')'
;;

let map_type_vars vars =
  (* When pretty-printing types, we want to show nicely formatted type variables (e.g. 'a, 'b etc.)
     rather than integers, or letters in the middle of the alphabet.

     So, collect together all of the type variables in the type, arrange them in alphabetical order,
     and assign an integer to each one. [pp_tv] is responsible for pretty-printing that int as a
     string. *)
  Set.to_list vars |> List.mapi ~f:(fun i tv -> tv, i) |> Var.Map.of_alist_exn
;;

let pp_poly formatter (ty : Poly.t) =
  let%tydi { quantifiers; body; constraints } = ty in
  (* Union the free type variables of [ty] with [quantifiers] in case we don't use any of the
     quantifiers. *)
  let quantifiers = Var.Set.of_list quantifiers in
  let tvs = map_type_vars (Set.union (free_type_vars body) quantifiers) in
  (* Don't show quantifiers if there aren't any. *)
  if not (Set.is_empty quantifiers)
  then (
    Format.pp_print_iter
      ~pp_sep:(fun formatter () -> Format.pp_print_string formatter " ")
      (fun f set -> Set.iter set ~f)
      (pp_tv ~tvs)
      formatter
      quantifiers;
    Format.pp_print_string formatter ". ");
  if not (List.is_empty constraints)
  then (
    Format.pp_print_list
      ~pp_sep:(fun formatter () -> Format.pp_print_string formatter ", ")
      (fun formatter { Constraint.type_class; arg } ->
        Format.pp_print_string formatter (Type_class_name.to_string type_class);
        Format.pp_print_string formatter " (";
        pp_type' formatter ~tvs arg;
        Format.pp_print_string formatter ")")
      formatter
      constraints;
    Format.pp_print_string formatter " => ");
  pp_type' formatter body ~tvs
;;

let pp formatter ty =
  let tvs = map_type_vars (free_type_vars ty) in
  pp_type' formatter ty ~tvs
;;
