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
  | Record of (Field_name.t Located.t * t) list (* | Sequence of t list*)
  | Match of
      { scrutinee : t
      ; cases : (Pattern.t * t) list
      }
[@@deriving sexp_of]

let const_int n ~loc = { desc = Const (Int (Int.to_string n)); loc }
let const_string s ~loc = { desc = Const (String s); loc }

let rec is_syntactic_value t =
  (* see: http://mlton.org/ValueRestriction *)
  match t.desc with
  | Var _ -> true
  | Const _ -> true
  | Lambda _ -> true
  | Tuple ts -> List.for_all ts ~f:is_syntactic_value
  | Record fields -> List.for_all fields ~f:(fun (_, t) -> is_syntactic_value t)
  | Construct _ -> true
  | Match _ -> false
  | Let _ -> false
  | Apply _ -> false
;;
