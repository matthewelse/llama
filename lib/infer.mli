open! Core

val type_of_let_binding
  :  Expression.t
  -> env:Type.Poly.t Ident.Map.t
  -> Type.Poly.t Or_error.t
