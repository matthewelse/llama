open! Core

module Name : sig
  include String_id.S

  module Built_in : sig
    val bool : t
    val int : t
    val string : t
    val unit : t
  end
end

module Var : sig
  type t [@@immediate]

  include Unique_id.Id with type t := t
end

type t =
  | Var of Var.t
  | Apply of Name.t * t list
  | Fun of t * t
  | Tuple of t list
[@@deriving sexp_of]

(** [unify t t'] raises if [t] is a type variable that occurs in [t']. *)
val unify : t -> t -> t Var.Map.t

val unify' : t -> t -> acc:t Var.Map.t -> t Var.Map.t
val free_type_vars : t -> Var.Set.t

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
end

val generalize : t -> env:Poly.t Ident.Map.t -> Poly.t
val subst : t -> replacements:t Var.Map.t -> t
