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

val constructor_exn : t -> Constructor.t -> loc:Span.t -> Type_name.t
val field_exn : t -> Field_name.t -> loc:Span.t -> Type_name.t
val type_declaration_exn : t -> Type_name.t -> loc:Span.t -> Type.Constructor.t
val value_exn : t -> Ident.t -> loc:Span.t -> Type.Poly.t
val type_class_exn : t -> Type_class_name.t -> loc:Span.t -> Type_class.t

(** Modifers *)

val with_constructors
  :  t
  -> ((Constructor.t * Span.t) * _) list
  -> type_name:Type_name.t
  -> t

val with_fields : t -> ((Field_name.t * Span.t) * _) list -> type_name:Type_name.t -> t
val with_type_class : t -> Type_class_name.t -> Type_class.t -> t
val with_type_declaration : t -> Type_name.t -> Type.Constructor.t -> loc:Span.t -> t
val with_var : t -> Ident.t -> Type.Poly.t -> t
val with_vars : t -> (Ident.t * Type_var.t) list -> t
val remove_var : t -> Ident.t -> t
val with_type_class_impl : t -> Type_class.Impl.t -> t
