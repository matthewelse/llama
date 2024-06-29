open! Core
open! Import

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
  | Apply (n, []) -> Format.pp_print_string formatter (Type_name.to_string n.value)
  | Apply (n, vars) ->
    Format.pp_print_char formatter '(';
    Format.pp_print_list
      ~pp_sep:(fun formatter () -> Format.pp_print_string formatter ", ")
      (pp_type' ~tvs)
      formatter
      vars;
    Format.pp_print_string formatter ") ";
    Format.pp_print_string formatter (Type_name.to_string n.value)
  | Fun (args, r) ->
    Format.pp_print_list
      ~pp_sep:(fun formatter () -> Format.pp_print_string formatter " -> ")
      (pp_type' ~tvs)
      formatter
      args;
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
  let%tydi { quantifiers; ty; constraints } = ty in
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
  if not (List.is_empty constraints)
  then (
    Format.pp_print_list
      ~pp_sep:(fun formatter () -> Format.pp_print_string formatter ", ")
      (fun formatter { Type.Constraint.type_class; arg } ->
        Format.pp_print_string formatter (Type_class_name.to_string type_class);
        Format.pp_print_string formatter " (";
        pp_type' formatter ~tvs arg;
        Format.pp_print_string formatter ")")
      formatter
      constraints;
    Format.pp_print_string formatter " => ");
  pp_type' formatter ty ~tvs
;;

let pp_type formatter ty =
  let tvs = map_type_vars (Type.free_type_vars ty) in
  pp_type' formatter ty ~tvs
;;

let rec pp_ast_type formatter (ty : Ast.Type.t) =
  match ty.desc with
  | Var tv -> Format.pp_print_string formatter tv
  | Apply (n, []) -> Format.pp_print_string formatter (Type_name.to_string n.value)
  | Apply (n, vars) ->
    Format.pp_print_char formatter '(';
    Format.pp_print_list
      ~pp_sep:(fun formatter () -> Format.pp_print_string formatter ", ")
      pp_ast_type
      formatter
      vars;
    Format.pp_print_string formatter ") ";
    Format.pp_print_string formatter (Type_name.to_string n.value)
  | Fun (args, r) ->
    Format.pp_print_list
      ~pp_sep:(fun formatter () -> Format.pp_print_string formatter " -> ")
      pp_ast_type
      formatter
      args;
    Format.pp_print_string formatter " -> ";
    pp_ast_type formatter r
  | Tuple ts ->
    Format.pp_print_char formatter '(';
    Format.pp_print_list
      ~pp_sep:(fun formatter () -> Format.pp_print_string formatter " * ")
      pp_ast_type
      formatter
      ts.value;
    Format.pp_print_char formatter ')'
;;

let pp_ast_polytype formatter (ty : Ast.Type.Poly.t) =
  let%tydi { quantifiers; ty } = ty in
  (* Don't show quantifiers if there aren't any. *)
  if not (List.is_empty quantifiers)
  then (
    Format.pp_print_list
      ~pp_sep:(fun formatter () -> Format.pp_print_string formatter " ")
      Format.pp_print_string
      formatter
      quantifiers;
    Format.pp_print_string formatter ". ");
  pp_ast_type formatter ty
;;

let pp_ident formatter ident = Format.pp_print_string formatter (Ident.to_string ident)

let pp_const formatter (const : Expression.Const.t) =
  match const with
  | Int i -> Format.pp_print_string formatter i
  | String s -> Format.pp_print_string formatter s
;;

let rec pp_pattern formatter (pattern : Pattern.t) =
  match pattern.desc with
  | Var ident -> pp_ident formatter ident.value
  | Construct (constructor, None) ->
    Format.pp_print_string formatter (Constructor.to_string constructor.value)
  | Construct (constructor, Some pattern) ->
    Format.pp_print_string formatter (Constructor.to_string constructor.value);
    Format.pp_print_char formatter ' ';
    pp_pattern formatter pattern
  | Tuple patterns ->
    Format.pp_print_char formatter '(';
    Format.pp_print_list
      ~pp_sep:(fun formatter () -> Format.pp_print_string formatter ", ")
      pp_pattern
      formatter
      patterns.value;
    Format.pp_print_char formatter ')'
;;

let rec pp_expr formatter (expr : Expression.t) =
  match expr.desc with
  | Var ident -> pp_ident formatter ident
  | Apply (f, arg) ->
    pp_expr formatter f;
    Format.pp_print_char formatter '(';
    Format.pp_print_list
      ~pp_sep:(fun formatter () -> Format.pp_print_string formatter ", ")
      pp_expr
      formatter
      arg.value;
    Format.pp_print_char formatter ')'
  | Lambda (args, body) ->
    Format.pp_print_string formatter "fun ";
    Format.pp_print_char formatter '(';
    Format.pp_print_list
      ~pp_sep:(fun formatter () -> Format.pp_print_string formatter ", ")
      pp_ident
      formatter
      args;
    Format.pp_print_char formatter ')';
    Format.pp_print_string formatter " -> ";
    pp_expr formatter body
  | Let { name; value; in_ } ->
    Format.pp_print_string formatter "let ";
    pp_ident formatter name;
    Format.pp_print_string formatter " = ";
    pp_expr formatter value;
    Format.pp_print_string formatter " in ";
    pp_expr formatter in_
  | Const c -> pp_const formatter c
  | Tuple es ->
    Format.pp_print_char formatter '(';
    Format.pp_print_list
      ~pp_sep:(fun formatter () -> Format.pp_print_string formatter ", ")
      pp_expr
      formatter
      es;
    Format.pp_print_char formatter ')'
  | Construct (constructor, arg) ->
    Format.pp_print_string formatter (Constructor.to_string constructor.value);
    (match arg with
     | None -> ()
     | Some arg ->
       Format.pp_print_string formatter " (";
       pp_expr formatter arg;
       Format.pp_print_string formatter ")")
  | Record fields ->
    Format.pp_print_char formatter '{';
    Format.pp_print_list
      ~pp_sep:(fun formatter () -> Format.pp_print_string formatter "; ")
      (fun formatter ({ Located.value = ident; _ }, expr) ->
        Format.pp_print_string formatter (Field_name.to_string ident);
        Format.pp_print_string formatter " = ";
        pp_expr formatter expr)
      formatter
      fields;
    Format.pp_print_char formatter '}'
  | Match { scrutinee; cases } ->
    Format.pp_print_string formatter "match ";
    pp_expr formatter scrutinee;
    Format.pp_print_string formatter " with\n";
    Format.pp_print_list
      ~pp_sep:(fun formatter () -> Format.pp_print_cut formatter ())
      (fun formatter (pattern, expr) ->
        Format.pp_print_string formatter "| ";
        pp_pattern formatter pattern;
        Format.pp_print_string formatter " -> ";
        pp_expr formatter expr)
      formatter
      cases;
    Format.pp_close_box formatter ()
;;

let pp_intrinsic formatter intrinsic =
  Format.pp_print_char formatter '"';
  Format.pp_print_string formatter (Intrinsic.Value.to_string intrinsic);
  Format.pp_print_char formatter '"'
;;

let pp_type_name formatter name =
  Format.pp_print_string formatter (Type_name.to_string name)
;;

let pp_record_field formatter ({ Located.value = ident; _ }, ty) =
  Format.pp_print_string formatter (Field_name.to_string ident);
  Format.pp_print_string formatter " : ";
  pp_ast_type formatter ty
;;

let pp_located pp_a formatter (located : _ Located.t) = pp_a formatter located.value

let pp_type_shape formatter (shape : Ast.Type_shape.t) =
  match shape with
  | Record { fields } ->
    Format.pp_print_char formatter '{';
    Format.pp_print_list
      ~pp_sep:(fun formatter () -> Format.pp_print_string formatter "; ")
      pp_record_field
      formatter
      fields;
    Format.pp_print_char formatter '}'
  | Variant { constructors } ->
    Format.pp_print_list
      ~pp_sep:(fun formatter () -> Format.pp_print_string formatter " ")
      (fun formatter ({ Located.value = name; _ }, ty) ->
        Format.pp_print_string formatter "| ";
        Format.pp_print_string formatter (Constructor.to_string name);
        match ty with
        | None -> ()
        | Some ty ->
          Format.pp_print_string formatter " of ";
          pp_ast_type formatter ty)
      formatter
      constructors
;;

let pp_type_args formatter type_params =
  if not (List.is_empty type_params)
  then (
    if List.length type_params > 1 then Format.pp_print_char formatter '(';
    Format.pp_print_list
      ~pp_sep:(fun formatter () -> Format.pp_print_string formatter ", ")
      (pp_located (fun formatter tv -> Format.pp_print_string formatter tv))
      formatter
      type_params;
    if List.length type_params > 1 then Format.pp_print_char formatter ')';
    Format.pp_print_string formatter " ")
;;

let pp_structure_item formatter (item : Ast.Structure_item.t) =
  match item with
  | Let { name; value; loc = _ } ->
    Format.pp_print_string formatter "let ";
    pp_ident formatter name.value;
    Format.pp_print_string formatter " = ";
    pp_expr formatter value
  | Intrinsic { name; intrinsic; type_; loc = _ } ->
    Format.pp_print_string formatter "intrinsic ";
    pp_ident formatter name.value;
    Format.pp_print_string formatter " : ";
    pp_ast_polytype formatter type_;
    Format.pp_print_string formatter " = ";
    pp_intrinsic formatter intrinsic.value
  | Type_class_declaration { name; arg; functions; constraints } ->
    Format.fprintf
      formatter
      "class %s (%s)%a : sig"
      (Type_class_name.to_string name.value)
      (Located.value arg)
      (fun formatter constraints ->
        if not (List.is_empty constraints)
        then (
          Format.pp_print_string formatter " where ";
          Format.pp_print_list
            ~pp_sep:(fun formatter () -> Format.pp_print_string formatter ", ")
            (fun formatter { Ast.Type_constraint.type_class; arg } ->
              Format.fprintf
                formatter
                "%s(%s)"
                (Type_class_name.to_string (Located.value type_class))
                (Located.value arg))
            formatter
            constraints))
      constraints;
    Format.pp_print_newline formatter ();
    Format.pp_print_list
      ~pp_sep:Format.pp_print_newline
      (fun formatter { Ast.Type_class_declaration.Function_decl.name; ty } ->
        Format.pp_print_string formatter [%string "  val %{name.value#Ident} : "];
        pp_ast_type formatter ty)
      formatter
      functions;
    Format.pp_print_newline formatter ();
    Format.pp_print_string formatter "end"
  | Type_class_implementation
      { name; for_ = type_constructor, args; functions; constraints } ->
    let format_for formatter (type_arg : _ Located.t) =
      Format.fprintf formatter "%s" (Located.value type_arg)
    in
    Format.fprintf
      formatter
      "impl %s (%a %s)%a : sig"
      (Type_class_name.to_string name.value)
      (Format.pp_print_list
         ~pp_sep:(fun formatter () -> Format.pp_print_string formatter ", ")
         format_for)
      args
      (Type_name.to_string type_constructor.value)
      (fun formatter constraints ->
        if not (List.is_empty constraints)
        then (
          Format.pp_print_string formatter " where ";
          Format.pp_print_list
            ~pp_sep:(fun formatter () -> Format.pp_print_string formatter ", ")
            (fun formatter { Ast.Type_constraint.type_class; arg } ->
              Format.fprintf
                formatter
                "%s(%s)"
                (Type_class_name.to_string (Located.value type_class))
                (Located.value arg))
            formatter
            constraints))
      constraints;
    Format.pp_print_newline formatter ();
    Format.pp_print_list
      ~pp_sep:Format.pp_print_newline
      (fun formatter { Ast.Type_class_implementation.Function_impl.name; value } ->
        Format.fprintf
          formatter
          "  let %s = %a"
          (Ident.to_string name.value)
          pp_expr
          value)
      formatter
      functions
  | Type_declaration { name = { value = name; _ }; type_params; type_shape; loc = _ } ->
    Format.pp_print_string formatter "type ";
    pp_type_args formatter type_params;
    pp_type_name formatter name;
    Format.pp_print_string formatter " = ";
    pp_type_shape formatter type_shape
;;

let pp_ast formatter (ast : Ast.t) =
  Format.pp_print_list
    ~pp_sep:(fun formatter () -> Format.pp_print_string formatter "\n")
    pp_structure_item
    formatter
    ast
;;

module For_testing = struct
  let pp_tv' = pp_tv'
end
