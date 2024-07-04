open! Core
open! Import

module Var : sig
  type t [@@immediate]

  include Unique_id.Id with type t := t
end

module Id : sig
  (** Used to uniquely identify nominally typed records and variants. *)

  type t [@@immediate]

  include Unique_id.Id with type t := t
end

module A : sig
  type apply = unit [@@deriving sexp_of]
  type fun_ = unit [@@deriving sexp_of]
  type tuple = unit [@@deriving sexp_of]

  type type_constructor =
    [ `Built_in
    | `Position of Span.t
    ]
  [@@deriving sexp_of]

  type var = unit [@@deriving sexp_of]
end

include Base_ast.Type.S with module Type_var := Var and module A := A

val var : Var.t -> t
val of_ast : Ast.Type.t -> var_mapping:Var.t String.Map.t -> t

module Constraint : sig
  type ty := t

  type t = Constraint.t =
    { type_class : Type_class_name.t
    ; arg : ty
    }
  [@@deriving sexp_of]
end

module Poly : sig
  type ty := t

  (** forall [quantifiers]. [constraints] => [ty] *)
  type t = Poly.t =
    { quantifiers : Var.t list
    ; body : ty
    ; constraints : Constraint.t list
    }
  [@@deriving sexp_of]

  (** [ty] with no quantifiers. *)
  val mono : ty -> t

  (** [init t] returns a monomorphic type with fresh free type variables for each of the quantified
      type variables. *)
  val init : t -> ty * Constraint.t list

  val subst : t -> replacements:ty Var.Map.t -> t
  val free_type_vars : t -> Var.Set.t
  val of_ast : Ast.Type.Poly.t -> var_mapping:Var.t String.Map.t -> t
end

module Constructor : sig
  type ty := t

  module Shape : sig
    type t =
      | Intrinsic of Intrinsic.Type.t
      | Record of
          { fields : ((Field_name.t * Span.t) * ty) list
          ; id : Id.t
          }
      | Variant of
          { constructors : ((Constructor.t * Span.t) * ty option) list
          ; id : Id.t
          }
    [@@deriving sexp_of]
  end

  type t =
    { shape : Shape.t
    ; args : Var.t list
    ; loc : [ `Built_in | `Position of Span.t ]
    }
  [@@deriving sexp_of]
end

val occurs : t -> var:Var.t -> bool
val free_type_vars : t -> Var.Set.t
val intrinsic : ?loc:[ `Built_in | `Position of Span.t ] -> Intrinsic.Type.t -> t

(** [const name] is a type with no type parameters. *)
val const : Type_name.t * [ `Built_in | `Position of Span.t ] -> t

val generalize : t -> env:Poly.t Ident.Map.t -> Poly.t
val subst : t -> replacements:t Var.Map.t -> t
val pp : Formatter.t -> t -> unit
val pp_poly : Formatter.t -> Poly.t -> unit
