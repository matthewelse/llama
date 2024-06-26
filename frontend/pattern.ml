open! Core
open! Import

type desc =
  | Construct of (Constructor.t Located.t * t option)
  | Tuple of t list Located.t
  | Var of Ident.t Located.t

and t =
  { loc : Span.t
  ; desc : desc
  }
[@@deriving sexp_of]
