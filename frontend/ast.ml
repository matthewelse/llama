open! Core
open! Import

module Type = struct
  type t =
    { desc : desc
    ; loc : Span.t
    }

  and desc =
    | Var of string
    | Intrinsic of Intrinsic.Type.t
    | Apply of (Type_name.t Located.t * t list)
    | Fun of (t list * t)
    | Tuple of t list Located.t
  [@@deriving sexp_of]

  module Poly = struct
    type ty = t [@@deriving sexp_of]

    type t =
      { quantifiers : string list
      ; ty : ty
      }
    [@@deriving sexp_of]
  end

  let rec free_type_vars t ~acc =
    match t.desc with
    | Var v -> v :: acc
    | Intrinsic _ -> acc
    | Apply (_, args) ->
      List.fold args ~init:acc ~f:(fun acc next -> free_type_vars next ~acc)
    | Fun (args, r) ->
      let acc = List.fold args ~init:acc ~f:(fun acc next -> free_type_vars next ~acc) in
      free_type_vars r ~acc
    | Tuple ts ->
      List.fold ts.value ~init:acc ~f:(fun acc next -> free_type_vars next ~acc)
  ;;

  let generalize t : Poly.t =
    let quantifiers = free_type_vars t ~acc:[] in
    { ty = t; quantifiers }
  ;;

  let const name = Apply (name, [])
end

module Type_shape = struct
  type t =
    | Alias of Type.t
    | Record of { fields : (Field_name.t Located.t * Type.t) list }
    | Variant of { constructors : (Constructor.t Located.t * Type.t option) list }
  [@@deriving sexp_of]
end

module Type_declaration = struct
  type t =
    { name : Type_name.t Located.t
    ; type_params : string Located.t list
    ; type_shape : Type_shape.t
    ; loc : Span.t
    }
  [@@deriving sexp_of]
end

module Value_intrinsic = struct
  type t =
    { name : Ident.t Located.t
    ; intrinsic : Intrinsic.Value.t Located.t
    ; type_ : Type.Poly.t
    ; loc : Span.t
    }
  [@@deriving sexp_of]
end

module Let_binding = struct
  type t =
    { name : Ident.t Located.t
    ; value : Expression.t
    ; loc : Span.t
    }
  [@@deriving sexp_of]
end

module Structure_item = struct
  type t =
    | Let of Let_binding.t
    | Intrinsic of Value_intrinsic.t
    | Type_declaration of Type_declaration.t
  [@@deriving sexp_of]
end

type t = Structure_item.t list [@@deriving sexp_of]
