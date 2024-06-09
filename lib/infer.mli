open! Core
open! Import

module Env : sig
  type t =
    { values : Type.Poly.t Ident.Map.t
    ; type_declarations : Type.Constructor.t Type_name.Map.t
    ; constructors : Type_name.t Constructor.Map.t
    ; fields : Type_name.t Field_name.Map.t
    }
  [@@deriving sexp_of]

  val empty : t
end

val type_of_let_binding : Expression.t -> Env.t -> Type.Poly.t Or_error.t
val type_ast : Ast.t -> Env.t Or_error.t
