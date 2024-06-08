open! Core

module Value = struct
  type t = Add_int [@@deriving variants, sexp_of]

  let to_string t = "%" ^ Variants.to_name t |> String.lowercase
end

module Type = struct
  type t =
    | Int
    | String
  [@@deriving enumerate, equal, variants, sexp_of]

  let to_string t = "%" ^ Variants.to_name t |> String.lowercase
end
