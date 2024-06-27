open! Core
open! Import

type t =
  { type_vars : Type.Var.t list
  ; members : Type.t Ident.Map.t
  }
[@@deriving sexp_of]
