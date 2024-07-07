open! Core
open! Import

type t =
  | Expression_should_have_type of Expression.t * Type.t
  | Pattern_should_have_type of Pattern.t * Type.t
  | Var_requires_type_class of Ident.t Located.t * Type_class_name.t
[@@deriving sexp_of]

let loc t =
  match t with
  | Expression_should_have_type (e, _) -> Expression.loc e
  | Pattern_should_have_type (p, _) -> Pattern.loc p
  | Var_requires_type_class ({ loc; _ }, _) -> loc
;;
