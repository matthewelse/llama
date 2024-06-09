open! Core

module Value : sig
  type t = Add_int [@@deriving variants, sexp_of]

  include Stringable.S with type t := t
end

module Type : sig
  type t =
    | Int
    | String
  [@@deriving enumerate, equal, variants, sexp_of]

  include Stringable.S with type t := t
end
