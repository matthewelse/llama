open! Core

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

let rec pp_type' formatter (ty : Type.t) ~tvs =
  match ty with
  | Var tv -> pp_tv formatter tv ~tvs
  | Apply (n, []) -> Format.pp_print_string formatter (Type.Name.to_string n)
  | Apply (n, vars) ->
    Format.pp_print_char formatter '(';
    Format.pp_print_list
      ~pp_sep:(fun formatter () -> Format.pp_print_string formatter ", ")
      (pp_type' ~tvs)
      formatter
      vars;
    Format.pp_print_string formatter ") ";
    Format.pp_print_string formatter (Type.Name.to_string n)
  | Fun (l, r) ->
    pp_type' formatter l ~tvs;
    Format.pp_print_string formatter " -> ";
    pp_type' formatter r ~tvs
  | Tuple ts ->
    Format.pp_print_char formatter '(';
    Format.pp_print_list
      ~pp_sep:(fun formatter () -> Format.pp_print_string formatter " * ")
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
  Set.to_list vars |> List.mapi ~f:(fun i tv -> tv, i) |> Type.Var.Map.of_alist_exn
;;

let pp_polytype formatter (ty : Type.Poly.t) =
  let%tydi { quantifiers; ty } = ty in
  (* Union the free type variables of [ty] with [quantifiers] in case we don't use any of the
     quantifiers. *)
  let tvs = map_type_vars (Set.union (Type.free_type_vars ty) quantifiers) in
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
  pp_type' formatter ty ~tvs
;;

let pp_type formatter ty =
  let tvs = map_type_vars (Type.free_type_vars ty) in
  pp_type' formatter ty ~tvs
;;

module For_testing = struct
  let pp_tv' = pp_tv'
end
