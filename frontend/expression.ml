open! Core
open! Import

module Const = struct
  type t =
    | Int of string
    | String of string
  [@@deriving sexp_of, variants]

  let intrinsic_type t : Intrinsic.Type.t =
    match t with
    | Int _ -> Int
    | String _ -> String
  ;;
end

type t =
  | Var of Ident.t
  | Apply of t * t list
  | Lambda of Ident.t list * t
  | Let of
      { name : Ident.t
      ; value : t
      ; in_ : t
      }
  | Const of Const.t
  | Tuple of t list
  | Construct of Constructor.t * t option
  | Record of (Field_name.t * t) list (* | Sequence of t list*)
[@@deriving sexp_of, variants]

let const_int n = Const (Int (Int.to_string n))
let const_string s = Const (String s)
