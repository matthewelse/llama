open! Core
open! Import

module Annotation : sig
  type t =
    | Expression_should_have_type of Expression.t * Type.t
    | Pattern_should_have_type of Pattern.t * Type.t
  [@@deriving sexp_of]
end

module Constraint : sig
  type 'a t = Same_type of Type.t * Type.t * 'a [@@deriving sexp_of]
end

module Annotations : sig
  type t = Annotation.t * Annotation.t list [@@deriving sexp_of]
end

type t [@@deriving sexp_of]

val empty : t
val merge : t -> t -> t
val to_list : t -> Annotations.t Constraint.t list
val infer : Expression.t -> env:Env.t -> (Type.t * t, Type_error.t) result

module For_testing : sig
  val check_pattern : Pattern.t -> Type.t -> env:Env.t -> (t * Env.t, Type_error.t) result
end
