open! Core

module Value = struct
  type t =
    | Int_add
    | Int_equal
    | Int_land
    | Int_neg
    | Ref_make
    | Ref_set
  [@@deriving enumerate, equal, variants, sexp_of]

  let to_string t =
    (* FIXME melse: consistent naming *)
    match t with
    | Int_add -> "%int_add"
    | Int_land -> "%int_land"
    | Int_equal -> "%int_equal"
    | Int_neg -> "%int_neg"
    | Ref_make -> "%ref_make"
    | Ref_set -> "%ref_set"
  ;;

  let of_string s =
    match s with
    | "%int_add" -> Int_add
    | "%int_equal" -> Int_equal
    | "%int_land" -> Int_land
    | "%int_neg" -> Int_neg
    | "%ref_make" -> Ref_make
    | "%ref_set" -> Ref_set
    | _ -> failwith [%string "Invalid value intrinsic %{s}"]
  ;;

  let%expect_test "[to_string]/[of_string] round-trip" =
    List.iter all ~f:(fun t -> assert (equal t (of_string (to_string t))))
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
