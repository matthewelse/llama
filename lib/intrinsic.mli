open! Core

module Value : sig
  type t = Add_int [@@deriving variants, sexp_of]

  val to_string : t -> string
end

module Type : sig
  type t =
    | Int
    | String
  [@@deriving enumerate, equal, variants, sexp_of]

  val to_string : t -> string
end
