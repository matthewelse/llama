open! Core
open! Import

type 'a t =
  { value : 'a
  ; loc : Span.t
  }
[@@deriving compare, fields ~getters, sexp_of]

val dummy : 'a -> 'a t
val value_equal : 'a t -> 'a t -> f:('a -> 'a -> bool) -> bool
val map : 'a t -> f:('a -> 'b) -> 'b t
