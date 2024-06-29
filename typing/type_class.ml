open! Core
open! Import

type t =
  { type_var : Type.Var.t
  ; members : Type.t Ident.Map.t
  }
[@@deriving sexp_of]

module Impl = struct
  type t =
    { constraints : Type_class_name.t list
    ; type_class : Type_class_name.t
    ; for_type : Type.t
    }
  [@@deriving sexp_of]
end
