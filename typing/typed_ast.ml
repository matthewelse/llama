open! Core
open! Import

module Type =
  Base_ast.Type.Make
    (Type_var)
    (struct
      type apply = unit [@@deriving sexp_of]
      type fun_ = unit [@@deriving sexp_of]
      type tuple = unit [@@deriving sexp_of]

      type type_constructor =
        [ `Built_in
        | `Position of Span.t
        ]
      [@@deriving sexp_of]

      type var = unit [@@deriving sexp_of]
    end)

include
  Base_ast.Expression.Make
    (struct
      type t = Type.t [@@deriving sexp_of]
      type var = Type_var.t [@@deriving sexp_of]
    end)
    (struct
      module Expression = struct
        type apply = unit [@@deriving sexp_of]
        type const = unit [@@deriving sexp_of]
        type construct = unit [@@deriving sexp_of]
        type constructor = unit [@@deriving sexp_of]
        type lambda = unit [@@deriving sexp_of]
        type lambda_arg = Type.t [@@deriving sexp_of]
        type let_ = unit [@@deriving sexp_of]
        type let_name = unit [@@deriving sexp_of]
        type match_ = unit [@@deriving sexp_of]
        type record = unit [@@deriving sexp_of]
        type record_field = unit [@@deriving sexp_of]
        type tapply = unit [@@deriving sexp_of]
        type tfun = unit [@@deriving sexp_of]
        type tuple = unit [@@deriving sexp_of]
        type var = unit [@@deriving sexp_of]
      end

      module Pattern = struct
        type construct = unit [@@deriving sexp_of]
        type constructor = unit [@@deriving sexp_of]
        type var = unit [@@deriving sexp_of]
        type tuple = unit [@@deriving sexp_of]
      end
    end)
