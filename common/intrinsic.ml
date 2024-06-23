open! Core

module Value = struct
  type t =
    | Add_int
    | Make_ref
    | Set_ref
  [@@deriving variants, sexp_of]

  let to_string t =
    match t with
    | Add_int -> "%add_int"
    | Make_ref -> "%make_ref"
    | Set_ref -> "%set_ref"
  ;;

  let of_string s =
    match s with
    | "%add_int" -> Add_int
    | "%make_ref" -> Make_ref
    | "%set_ref" -> Set_ref
    | _ -> failwith [%string "Invalid value intrinsic %{s}"]
  ;;
end

module Type = struct
  type t =
    | Int
    | String
    | Ref
  [@@deriving enumerate, equal, variants, sexp_of]

  let to_string t =
    match t with
    | Int -> "%int"
    | String -> "%string"
    | Ref -> "%ref"
  ;;

  let of_string s =
    match s with
    | "%int" -> Int
    | "%string" -> String
    | "%ref" -> Ref
    | _ -> failwith [%string "Invalid type intrinsic %{s}"]
  ;;
end
