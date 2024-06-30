open! Core
open! Import

type t =
  { values : Type.Poly.t Ident.Map.t
  ; type_declarations : Type.Constructor.t Type_name.Map.t
  ; constructors : Type_name.t Constructor.Map.t
  ; fields : Type_name.t Field_name.Map.t
  ; type_classes : Type_class.t Type_class_name.Map.t
  ; type_class_implementations : Type_class.Impl.t list
  }
[@@deriving sexp_of]

val empty : unit -> t

(** Accessors *)

val constructor : t -> Constructor.t -> loc:Span.t -> (Type_name.t, Type_error.t) result
val field : t -> Field_name.t -> loc:Span.t -> (Type_name.t, Type_error.t) result

val type_declaration
  :  t
  -> Type_name.t
  -> loc:Span.t
  -> (Type.Constructor.t, Type_error.t) result

val value : t -> Ident.t -> loc:Span.t -> (Type.Poly.t, Type_error.t) result

val type_class
  :  t
  -> Type_class_name.t
  -> loc:Span.t
  -> (Type_class.t, Type_error.t) result

(** Modifers *)

val with_constructors
  :  t
  -> ((Constructor.t * Span.t) * _) list
  -> type_name:Type_name.t
  -> (t, Type_error.t) result

val with_fields
  :  t
  -> ((Field_name.t * Span.t) * _) list
  -> type_name:Type_name.t
  -> (t, Type_error.t) result

val with_type_class : t -> Type_class_name.t -> Type_class.t -> t
val with_type_declaration : t -> Type_name.t -> Type.Constructor.t -> t
val with_var : t -> Ident.t -> Type.Poly.t -> t
val with_vars : t -> (Ident.t * Type.Var.t) list -> t
val remove_var : t -> Ident.t -> t
val with_type_class_impl : t -> Type_class.Impl.t -> t
