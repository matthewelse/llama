open! Core

module Type_shape = struct
  type t =
    | Alias of Type.t
    | Record of { fields : (Field_name.t * Type.t) list }
    | Variant of { constructors : (Constructor.t * Type.t option) list }
  [@@deriving sexp_of]
end

module Type_declaration = struct
  type t =
    { type_params : Type.Var.t list
    ; type_shape : Type_shape.t
    ; type_vars : string Type.Var.Map.t
    }
  [@@deriving sexp_of]
end

module Structure_item = struct
  type t =
    | Let of
        { name : Ident.t
        ; value : Expression.t
        }
    | Intrinsic of
        { name : Ident.t
        ; intrinsic : Intrinsic.Value.t
        ; type_ : Type.Poly.t
        }
    | Type_declaration of
        { name : Type.Name.t
        ; type_declaration : Type_declaration.t
        }
  [@@deriving sexp_of]
end

type t = Structure_item.t list [@@deriving sexp_of]
