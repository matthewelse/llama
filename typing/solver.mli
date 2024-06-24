open! Core
open! Import

type t [@@deriving sexp_of]

val create : unit -> t
val solve : t -> Constraints.t -> env:Env.t -> (Env.t, Type_error.t) result
val normalize_ty : t -> Type.t -> env:Env.t -> (Type.t, Type_error.t) result
