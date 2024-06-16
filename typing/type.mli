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

type t =
  | Var of Var.t
  | Apply of Type_name.t * t list
  | Fun of t list * t
  | Tuple of t list
  | Intrinsic of Intrinsic.Type.t
[@@deriving sexp_of]

val of_ast : Ast.Type.t -> var_mapping:Var.t String.Map.t -> t

module Poly : sig
  type ty := t

  (** forall [quantifiers]. [ty] *)
  type t =
    { quantifiers : Var.Set.t
    ; ty : ty
    }
  [@@deriving sexp_of]

  (** [init t] returns a monomorphic type with fresh free type variables for each of the quantified
      type variables. *)
  val init : t -> ty

  val subst : t -> replacements:ty Var.Map.t -> t
  val free_type_vars : t -> Var.Set.t
  val of_ast : Ast.Type.Poly.t -> var_mapping:Var.t String.Map.t -> t
end

module Constructor : sig
  type ty := t

  module Shape : sig
    type t =
      | Alias of ty
      | Record of
          { fields : (Field_name.t * ty) list
          ; id : Id.t
          }
      | Variant of
          { constructors : (Constructor.t * ty option) list
          ; id : Id.t
          }
    [@@deriving sexp_of]
  end

  type t =
    { shape : Shape.t
    ; args : Var.t list
    }
  [@@deriving sexp_of]
end

(** [unify t t'] raises if [t] is a type variable that occurs in [t']. *)
val unify : t -> t -> tyenv:Constructor.t Type_name.Map.t -> t Var.Map.t

val unify'
  :  t
  -> t
  -> tyenv:Constructor.t Type_name.Map.t
  -> acc:t Var.Map.t
  -> t Var.Map.t

val free_type_vars : t -> Var.Set.t

(** [const name] is a type with no type parameters. *)
val const : Type_name.t -> t

val intrinsic : Intrinsic.Type.t -> t
val generalize : t -> env:Poly.t Ident.Map.t -> Poly.t
val subst : t -> replacements:t Var.Map.t -> t