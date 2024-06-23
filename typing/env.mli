open! Core
open! Import

type t =
  { values : Type.Poly.t Ident.Map.t
  ; type_declarations : Type.Constructor.t Type_name.Map.t
  ; constructors : Type_name.t Constructor.Map.t
  ; fields : Type_name.t Field_name.Map.t
  }
[@@deriving sexp_of]

val empty : t

(** Accessors *)

val constructor : t -> Constructor.t -> Type_name.t Or_error.t
val field : t -> Field_name.t -> Type_name.t Or_error.t
val type_declaration : t -> Type_name.t -> Type.Constructor.t Or_error.t
val value : t -> Ident.t -> Type.Poly.t Or_error.t

(** Modifers *)

val with_constructors : t -> (Constructor.t * _) list -> type_name:Type_name.t -> t
val with_fields : t -> (Field_name.t * _) list -> type_name:Type_name.t -> t Or_error.t
val with_type_declaration : t -> Type_name.t -> Type.Constructor.t -> t
val with_var : t -> Ident.t -> Type.Poly.t -> t
val with_vars : t -> (Ident.t * Type.Var.t) list -> t
