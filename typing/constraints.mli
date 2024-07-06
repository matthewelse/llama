open! Core
open! Import

module Annotation : sig
  type t =
    | Expression_should_have_type of Expression.t * Type.t
    | Pattern_should_have_type of Pattern.t * Type.t
    | Var_requires_type_class of Ident.t Located.t * Type_class_name.t
  [@@deriving sexp_of]

  val loc : t -> Span.t
end

module Constraint : sig
  type 'a t =
    | Same_type of Type.t * Type.t * 'a
    | Implements_type_class of Type_class_name.t * Type.t * 'a
  [@@deriving sexp_of]
end

module Annotations : sig
  type t = ( :: ) of Annotation.t * Annotation.t list [@@deriving sexp_of]

  val primary_loc : t -> Span.t
end

type t [@@deriving sexp_of]

module Gen_out : sig
  type constraints := t

  type 'ast t =
    { constraints : constraints
    ; typed_ast : 'ast
    }
end

val empty : t
val merge : t -> t -> t
val to_list : t -> Annotations.t Constraint.t list
val infer : Expression.t -> env:Env.t -> Typed_ast.Expression.t Gen_out.t * Type.t

module For_testing : sig
  val check_pattern
    :  Pattern.t
    -> Type.t
    -> env:Env.t
    -> Typed_ast.Pattern.t Gen_out.t * Env.t
end
