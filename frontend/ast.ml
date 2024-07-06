open! Core
open! Import

module Ast =
  Llama_common.Base_ast.Make
    (String)
    (struct
      module Expression = struct
        type apply = Span.t [@@deriving sexp_of]
        type const = Span.t [@@deriving sexp_of]
        type construct = Span.t [@@deriving sexp_of]
        type constructor = Span.t [@@deriving sexp_of]
        type lambda = Span.t [@@deriving sexp_of]
        type lambda_arg = Span.t [@@deriving sexp_of]
        type let_ = Span.t [@@deriving sexp_of]
        type let_name = Span.t [@@deriving sexp_of]
        type match_ = Span.t [@@deriving sexp_of]
        type record = Span.t [@@deriving sexp_of]
        type record_field = Span.t [@@deriving sexp_of]
        type tuple = Span.t [@@deriving sexp_of]
        type tapply = Nothing.t [@@deriving sexp_of]
        type tfun = Nothing.t [@@deriving sexp_of]
        type var = Span.t [@@deriving sexp_of]
      end

      module Pattern = struct
        type construct = Span.t [@@deriving sexp_of]
        type constructor = Span.t [@@deriving sexp_of]
        type tuple = Span.t [@@deriving sexp_of]
        type var = Span.t [@@deriving sexp_of]
      end

      module Type = struct
        type var = Span.t [@@deriving sexp_of]
        type apply = Span.t [@@deriving sexp_of]
        type fun_ = Span.t [@@deriving sexp_of]
        type tuple = Span.t [@@deriving sexp_of]
        type type_constructor = Span.t [@@deriving sexp_of]
      end
    end)

let pp_const formatter (const : Ast.Const.t) =
  match const with
  | Int i -> Format.pp_print_string formatter i
  | String s -> Format.pp_print_string formatter s
;;

module Pattern = struct
  include Ast.Pattern

  let loc t =
    match t with
    | Var (_, loc) | Construct (_, loc) | Tuple (_, loc) -> loc
  ;;

  let rec pp formatter t =
    match t with
    | Var (ident, _) -> Ident.pp formatter ident
    | Construct (((constructor, _), None), _) ->
      Format.pp_print_string formatter (Constructor.to_string constructor)
    | Construct (((constructor, _), Some pattern), _) ->
      Format.pp_print_string formatter (Constructor.to_string constructor);
      Format.pp_print_char formatter ' ';
      pp formatter pattern
    | Tuple (patterns, _) ->
      Format.pp_print_char formatter '(';
      Format.pp_print_list
        ~pp_sep:(fun formatter () -> Format.pp_print_string formatter ", ")
        pp
        formatter
        patterns;
      Format.pp_print_char formatter ')'
  ;;
end

module Expression = struct
  include Ast.Expression

  let rec is_syntactic_value t =
    (* see: http://mlton.org/ValueRestriction *)
    match t with
    | Var _ -> true
    | Const _ -> true
    | Lambda _ -> true
    | Tuple (ts, _) -> List.for_all ts ~f:is_syntactic_value
    | Record (fields, _) -> List.for_all fields ~f:(fun (_, t) -> is_syntactic_value t)
    | Construct _ -> true
    | Match _ -> false
    | Let _ -> false
    | Apply _ -> false
    | TFun _ | TApply _ -> .
  ;;

  let loc t =
    match t with
    | Var (_, loc)
    | Apply (_, loc)
    | Lambda (_, loc)
    | Let { annotation = loc; _ }
    | Const (_, loc)
    | Tuple (_, loc)
    | Construct (_, loc)
    | Record (_, loc)
    | Match { annotation = loc; _ } -> loc
  ;;

  let rec pp formatter t =
    match t with
    | Var (ident, _) -> Ident.pp formatter ident
    | Apply ((f, arg), _) ->
      pp formatter f;
      Format.pp_print_char formatter '(';
      Format.pp_print_list
        ~pp_sep:(fun formatter () -> Format.pp_print_string formatter ", ")
        pp
        formatter
        arg;
      Format.pp_print_char formatter ')'
    | Lambda ((args, body), _) ->
      Format.pp_print_string formatter "fun ";
      Format.pp_print_char formatter '(';
      Format.pp_print_list
        ~pp_sep:(fun formatter () -> Format.pp_print_string formatter ", ")
        (fun formatter (ident, _) -> Ident.pp formatter ident)
        formatter
        args;
      Format.pp_print_char formatter ')';
      Format.pp_print_string formatter " -> ";
      pp formatter body
    | Let { name; value; in_; annotation = _ } ->
      Format.pp_print_string formatter "let ";
      Ident.pp formatter (fst name);
      Format.pp_print_string formatter " = ";
      pp formatter value;
      Format.pp_print_string formatter " in ";
      pp formatter in_
    | Const (c, _) -> pp_const formatter c
    | Tuple (es, _) ->
      Format.pp_print_char formatter '(';
      Format.pp_print_list
        ~pp_sep:(fun formatter () -> Format.pp_print_string formatter ", ")
        pp
        formatter
        es;
      Format.pp_print_char formatter ')'
    | Construct (((constructor, _), arg), _) ->
      Format.pp_print_string formatter (Constructor.to_string constructor);
      (match arg with
       | None -> ()
       | Some arg ->
         Format.pp_print_string formatter " (";
         pp formatter arg;
         Format.pp_print_string formatter ")")
    | Record (fields, _) ->
      Format.pp_print_char formatter '{';
      Format.pp_print_list
        ~pp_sep:(fun formatter () -> Format.pp_print_string formatter "; ")
        (fun formatter ((ident, _), expr) ->
          Format.pp_print_string formatter (Field_name.to_string ident);
          Format.pp_print_string formatter " = ";
          pp formatter expr)
        formatter
        fields;
      Format.pp_print_char formatter '}'
    | Match { scrutinee; cases; annotation = _ } ->
      Format.pp_print_string formatter "match ";
      pp formatter scrutinee;
      Format.pp_print_string formatter " with\n";
      Format.pp_print_list
        ~pp_sep:(fun formatter () -> Format.pp_print_cut formatter ())
        (fun formatter (pattern, expr) ->
          Format.pp_print_string formatter "| ";
          Pattern.pp formatter pattern;
          Format.pp_print_string formatter " -> ";
          pp formatter expr)
        formatter
        cases;
      Format.pp_close_box formatter ()
    | TFun _ | TApply _ -> .
  ;;
end

module Type = Ast.Type
module Const = Ast.Const

module Type_shape = struct
  type t =
    | Record of { fields : ((Field_name.t * Span.t) * Type.t) list }
    | Variant of { constructors : ((Constructor.t * Span.t) * Type.t option) list }
  [@@deriving sexp_of]
end

module Type_declaration = struct
  type t =
    { name : Type_name.t * Span.t
    ; type_params : (string * Span.t) list
    ; type_shape : Type_shape.t
    ; loc : Span.t
    }
  [@@deriving sexp_of]
end

module Value_intrinsic = struct
  type t =
    { name : Ident.t * Span.t
    ; intrinsic : Intrinsic.Value.t * Span.t
    ; type_ : Type.Poly.t
    ; loc : Span.t
    }
  [@@deriving sexp_of]
end

module Let_binding = struct
  type t =
    { name : Ident.t * Span.t
    ; value : Expression.t
    ; loc : Span.t
    }
  [@@deriving sexp_of]
end

module Type_constraint = struct
  type t =
    { type_class : Type_class_name.t * Span.t
    ; arg : string * Span.t
    }
  [@@deriving sexp_of]
end

module Type_class_declaration = struct
  module Function_decl = struct
    type t =
      { name : Ident.t * Span.t (* TODO: constraints *)
      ; ty : Type.t
      }
    [@@deriving sexp_of]
  end

  type t =
    { name : Type_class_name.t * Span.t
    ; arg : string * Span.t
    ; functions : Function_decl.t list
    ; constraints : Type_constraint.t list
    }
  [@@deriving sexp_of]
end

module Type_class_implementation = struct
  module Function_impl = struct
    type t =
      { name : Ident.t * Span.t
      ; value : Expression.t
      }
    [@@deriving sexp_of]
  end

  type t =
    { name : Type_class_name.t * Span.t
    ; for_ : (Type_name.t * Span.t) * (string * Span.t) list
    ; functions : Function_impl.t list
    ; constraints : Type_constraint.t list
    }
  [@@deriving sexp_of]
end

module Structure_item = struct
  type t =
    | Let of Let_binding.t
    | Intrinsic of Value_intrinsic.t
    | Type_class_declaration of Type_class_declaration.t
    | Type_class_implementation of Type_class_implementation.t
    | Type_declaration of Type_declaration.t
  [@@deriving sexp_of]
end

type t = Structure_item.t list [@@deriving sexp_of]
