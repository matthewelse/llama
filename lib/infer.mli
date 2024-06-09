open! Core
open! Import

val type_of_let_binding
  :  Expression.t
  -> env:Type.Poly.t Ident.Map.t
  -> tyenv:Type.Constructor.t Type_name.Map.t
  -> constructors:Type_name.t Constructor.Map.t
  -> fields:Type_name.t Field_name.Map.t
  -> Type.Poly.t Or_error.t

val type_ast
  :  Ast.t
  -> (Type.Poly.t Ident.Map.t
     * Type.Constructor.t Type_name.Map.t
     * Type_name.t Constructor.Map.t
     * Type_name.t Field_name.Map.t)
       Or_error.t
