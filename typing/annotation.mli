open! Core
open! Import

type t =
  | Expression_should_have_type of Expression.t * Type.t
  | Pattern_should_have_type of Pattern.t * Type.t
  | Var_requires_type_class of Ident.t Located.t * Type_class_name.t
[@@deriving sexp_of]

val loc : t -> Span.t
