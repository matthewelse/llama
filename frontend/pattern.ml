open! Core
open! Import

type t =
  | Construct of (Constructor.t Located.t * t option)
  | Tuple of t list
  | Var of Ident.t Located.t
[@@deriving sexp_of]
