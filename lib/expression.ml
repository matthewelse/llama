open! Core

module Const = struct
  type t =
    | Int of int
    | String of string
  [@@deriving sexp_of, variants]

  let intrinsic_type t : Intrinsic.Type.t =
    match t with
    | Int _ -> Int
    | String _ -> String
  ;;

  let type_of t : Type.t = Type.intrinsic (intrinsic_type t)
end

type t =
  | Var of Ident.t
  | Apply of t * t
  | Lambda of Ident.t * t
  | Let of
      { name : Ident.t
      ; value : t
      ; in_ : t
      }
  | Const of Const.t
  | Tuple of t list
  | Construct of Constructor.t * t option
[@@deriving sexp_of, variants]

let const_int n = Const (Int n)
let const_string s = Const (String s)
