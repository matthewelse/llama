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
  { desc : desc
  ; loc : Span.t
  }

and desc =
  | Var of Ident.t
  | Apply of t * t list Located.t
  | Lambda of Ident.t list * t
  | Let of
      { name : Ident.t
      ; value : t
      ; in_ : t
      }
  | Const of Const.t
  | Tuple of t list
  | Construct of Constructor.t Located.t * t option
  | Record of (Field_name.t Located.t * t) list
  | Match of
      { scrutinee : t
      ; cases : (Pattern.t * t) list
      }
[@@deriving sexp_of]

val const_int : int -> loc:Span.t -> t
val const_string : string -> loc:Span.t -> t
val is_syntactic_value : t -> bool
