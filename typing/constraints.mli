open! Core
open! Import

module Constraint : sig
  type t = Same_type of Type.t * Type.t * string [@@deriving sexp_of]
end

type t [@@deriving sexp_of]

val empty : t
val merge : t -> t -> t
val to_list : t -> Constraint.t list
val infer : Expression.t -> env:Env.t -> (Type.t * t) Or_error.t

module For_testing : sig
  val check_pattern : Pattern.t -> Type.t -> env:Env.t -> (t * Env.t) Or_error.t
end
