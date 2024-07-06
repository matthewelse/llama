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

type 'ast t_generic =
  { constraints : Annotation.t Nonempty_list.t Constraint.t list
  ; typed_ast : 'ast
  }

type t = Typed_ast.Expression.t t_generic [@@deriving sexp_of]

(* FIXME: The whole name of this module, and the name of this function don't really make sense --
   really what we're doing is generating type constraints, and the typed ast at the same time. *)

val constraints : t -> Annotation.t Nonempty_list.t Constraint.t list
val infer : Expression.t -> env:Env.t -> t * Type.t

module For_testing : sig
  val check_pattern
    :  Pattern.t
    -> Type.t
    -> env:Env.t
    -> Typed_ast.Pattern.t t_generic * Env.t
end
