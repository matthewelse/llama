open! Core
open! Import

type t [@@deriving sexp_of]

val create : unit -> t
val solve : t -> Constraints.t -> env:Env.t -> Env.t Or_error.t
val normalize_ty : t -> Type.t -> env:Env.t -> Type.t Or_error.t
