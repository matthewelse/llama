open! Core

module Value : sig
  type t =
    | Add_int
    | Make_ref
    | Set_ref
    (** FIXME: This probably shouldn't be a special case - we should just allow first-class
        mutability *)
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
end
