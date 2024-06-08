open! Core

module Const : sig
  type t =
    | Int of int
    | String of string
  [@@deriving sexp_of]

  val type_of : t -> Type.t
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

val const_int : int -> t
val const_string : string -> t
