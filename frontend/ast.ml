open! Core
open! Import

module Type = struct
  type t =
    | Var of string
    | Intrinsic of Intrinsic.Type.t
    | Apply of (Type_name.t * t list)
    | Fun of (t list * t)
    | Tuple of t list
  [@@deriving sexp_of]

  module Poly = struct
    type ty = t [@@deriving sexp_of]

    type t =
      { quantifiers : string list
      ; ty : ty
      }
    [@@deriving sexp_of]
  end

  let const name = Apply (name, [])
end

module Type_shape = struct
  type t =
    | Alias of Type.t
    | Record of { fields : (Field_name.t * Type.t) list }
    | Variant of { constructors : (Constructor.t * Type.t option) list }
  [@@deriving sexp_of]
end

module Type_declaration = struct
  type t =
    { name : Type_name.t
    ; type_params : string list
    ; type_shape : Type_shape.t
    }
  [@@deriving sexp_of]
end

module Value_intrinsic = struct
  type t =
    { name : Ident.t
    ; intrinsic : Intrinsic.Value.t
    ; type_ : Type.Poly.t
    }
  [@@deriving sexp_of]
end

module Let_binding = struct
  type t =
    { name : Ident.t
    ; value : Expression.t
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
