open! Core

module Value = struct
  type t = Add_int [@@deriving variants, sexp_of]

  let to_string t =
    match t with
    | Add_int -> "%add_int"
  ;;

  let of_string s =
    match s with
    | "%add_int" -> Add_int
    | _ -> failwith [%string "Invalid value intrinsic %{s}"]
  ;;
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
