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

  let to_string t =
    match t with
    | Int -> "%int"
    | String -> "%string"
  ;;

  let of_string s =
    match s with
    | "%int" -> Int
    | "%string" -> String
    | _ -> failwith [%string "Invalid type intrinsic %{s}"]
  ;;
end
