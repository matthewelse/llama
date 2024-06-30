open! Core

module Value : sig
  type t =
    | Int_add
    | Int_equal
    | Int_land
    | Int_neg
    | Ref_make
    | Ref_set
    (** FIXME: [Ref_make]/[Ref_set] probably shouldn't be a special case - we should just allow
        first-class mutability *)
  [@@deriving variants, sexp_of]

  include Stringable.S with type t := t
end

module Type : sig
  type t =
    | Bool
    | Int
    | Ref
    | String
  [@@deriving enumerate, equal, variants, sexp_of]

  include Stringable.S with type t := t

  val type_name : t -> Type_name.t
  val arity : t -> int
end
