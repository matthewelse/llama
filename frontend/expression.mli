open! Core
open! Import

module Const : sig
  type t =
    | Int of string
    | String of string
  [@@deriving sexp_of]

  val intrinsic_type : t -> Intrinsic.Type.t
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

val const_int : int -> t
val const_string : string -> t
