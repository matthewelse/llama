open! Core
open! Import

module Constraint : sig
  type t =
    | Same_type of Type.t * Type.t * Annotation.t Nonempty_list.t
    | Implements_type_class of Type_class_name.t * Type.t * Annotation.t Nonempty_list.t
  [@@deriving sexp_of]
end

type t [@@deriving sexp_of]

val create : unit -> t
val solve : t -> Constraint.t list -> env:Env.t -> (Env.t, Type_error.t) result
val normalize_ty : t -> Type.t -> env:Env.t -> (Type.t, Type_error.t) result

val constraints
  :  t
  -> env:Env.t
  -> ((Type_class_name.t * Type.t list) list, Type_error.t) result
