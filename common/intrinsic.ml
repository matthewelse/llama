open! Core

module Value = struct
  type t =
    | Add_int
    | Int_equal
    | Make_ref
    | Set_ref
  [@@deriving variants, sexp_of]

  let to_string t =
    match t with
    | Add_int -> "%add_int"
    | Int_equal -> "%int_equal"
    | Make_ref -> "%make_ref"
    | Set_ref -> "%set_ref"
  ;;

  let of_string s =
    match s with
    | "%add_int" -> Add_int
    | "%int_equal" -> Int_equal
    | "%make_ref" -> Make_ref
    | "%set_ref" -> Set_ref
    | _ -> failwith [%string "Invalid value intrinsic %{s}"]
  ;;
end

module Type = struct
  type t =
    | Bool
    | Int
    | Ref
    | String
  [@@deriving enumerate, equal, variants, sexp_of]

  let to_string t =
    match t with
    | Bool -> "bool"
    | Int -> "int"
    | Ref -> "ref"
    | String -> "string"
  ;;

  let of_string s =
    match s with
    | "bool" -> Bool
    | "int" -> Int
    | "ref" -> Ref
    | "string" -> String
    | _ -> failwith [%string "Invalid type intrinsic %{s}"]
  ;;

  let type_name t = Type_name.of_string (to_string t)

  let arity = function
    | Bool | Int | String -> 0
    | Ref -> 1
  ;;
end
