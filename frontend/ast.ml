open! Core
open! Import

module Ast =
  Llama_common.Base_ast.Make
    (String)
    (struct
      module Expression = struct
        type apply = Span.t [@@deriving sexp_of]
        type const = Span.t [@@deriving sexp_of]
        type construct = Span.t [@@deriving sexp_of]
        type constructor = Span.t [@@deriving sexp_of]
        type lambda = Span.t [@@deriving sexp_of]
        type let_ = Span.t [@@deriving sexp_of]
        type match_ = Span.t [@@deriving sexp_of]
        type record = Span.t [@@deriving sexp_of]
        type record_field = Span.t [@@deriving sexp_of]
        type tuple = Span.t [@@deriving sexp_of]
        type var = Span.t [@@deriving sexp_of]
      end

      module Pattern = struct
        type construct = Span.t [@@deriving sexp_of]
        type constructor = Span.t [@@deriving sexp_of]
        type tuple = Span.t [@@deriving sexp_of]
        type var = Span.t [@@deriving sexp_of]
      end

      module Type = struct
        type var = Span.t [@@deriving sexp_of]
        type apply = Span.t [@@deriving sexp_of]
        type fun_ = Span.t [@@deriving sexp_of]
        type tuple = Span.t [@@deriving sexp_of]
        type type_constructor = Span.t [@@deriving sexp_of]
      end
    end)

module Pattern = struct
  include Ast.Pattern

  let loc t =
    match t with
    | Var (_, loc) | Construct (_, loc) | Tuple (_, loc) -> loc
  ;;
end

module Expression = struct
  include Ast.Expression

  let loc t =
    match t with
    | Var (_, loc)
    | Apply (_, loc)
    | Lambda (_, loc)
    | Let { annotation = loc; _ }
    | Const (_, loc)
    | Tuple (_, loc)
    | Construct (_, loc)
    | Record (_, loc)
    | Match { annotation = loc; _ } -> loc
  ;;
end

module Type = Ast.Type
module Const = Ast.Const

module Type_shape = struct
  type t =
    | Record of { fields : ((Field_name.t * Span.t) * Type.t) list }
    | Variant of { constructors : ((Constructor.t * Span.t) * Type.t option) list }
  [@@deriving sexp_of]
end

module Type_declaration = struct
  type t =
    { name : Type_name.t * Span.t
    ; type_params : (string * Span.t) list
    ; type_shape : Type_shape.t
    ; loc : Span.t
    }
  [@@deriving sexp_of]
end

module Value_intrinsic = struct
  type t =
    { name : Ident.t * Span.t
    ; intrinsic : Intrinsic.Value.t * Span.t
    ; type_ : Type.Poly.t
    ; loc : Span.t
    }
  [@@deriving sexp_of]
end

module Let_binding = struct
  type t =
    { name : Ident.t * Span.t
    ; value : Expression.t
    ; loc : Span.t
    }
  [@@deriving sexp_of]
end

module Type_constraint = struct
  type t =
    { type_class : Type_class_name.t * Span.t
    ; arg : string * Span.t
    }
  [@@deriving sexp_of]
end

module Type_class_declaration = struct
  module Function_decl = struct
    type t =
      { name : Ident.t * Span.t (* TODO: constraints *)
      ; ty : Type.t
      }
    [@@deriving sexp_of]
  end

  type t =
    { name : Type_class_name.t * Span.t
    ; arg : string * Span.t
    ; functions : Function_decl.t list
    ; constraints : Type_constraint.t list
    }
  [@@deriving sexp_of]
end

module Type_class_implementation = struct
  module Function_impl = struct
    type t =
      { name : Ident.t * Span.t
      ; value : Expression.t
      }
    [@@deriving sexp_of]
  end

  type t =
    { name : Type_class_name.t * Span.t
    ; for_ : (Type_name.t * Span.t) * (string * Span.t) list
    ; functions : Function_impl.t list
    ; constraints : Type_constraint.t list
    }
  [@@deriving sexp_of]
end

module Structure_item = struct
  type t =
    | Let of Let_binding.t
    | Intrinsic of Value_intrinsic.t
    | Type_class_declaration of Type_class_declaration.t
    | Type_class_implementation of Type_class_implementation.t
    | Type_declaration of Type_declaration.t
  [@@deriving sexp_of]
end

type t = Structure_item.t list [@@deriving sexp_of]
