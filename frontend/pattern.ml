open! Core
open! Import

type t =
  | Construct of (Constructor.t * t option)
  | Tuple of t list
  | Var of Ident.t
[@@deriving sexp_of]
