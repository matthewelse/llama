open! Core

val type_of_let_binding
  :  Expression.t
  -> env:Type.Poly.t Ident.Map.t
  -> tyenv:Type.Constructor.t Type.Name.Map.t
  -> constructors:Type.Name.t Constructor.Map.t
  -> fields:Type.Name.t Field_name.Map.t
  -> Type.Poly.t Or_error.t

val type_ast
  :  Ast.t
  -> (Type.Poly.t Ident.Map.t
     * Type.Constructor.t Type.Name.Map.t
     * Type.Name.t Constructor.Map.t
     * Type.Name.t Field_name.Map.t)
       Or_error.t
