open! Core

module Const = struct
  type t =
    | Int of int
    | String of string
  [@@deriving sexp_of, variants]

  let type_name t =
    match t with
    | Int _ -> Type.Name.Built_in.int
    | String _ -> Type.Name.Built_in.string
  ;;

  let type_of t : Type.t = Apply (type_name t, [])
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
[@@deriving sexp_of, variants]

let const_int n = Const (Int n)
let const_string s = Const (String s)
