open! Core

module Annotated = struct
  type ('value, 'annotation) t = 'value * 'annotation [@@deriving sexp_of]
end

module Type_annotation = struct
  module type S = sig
    type apply [@@deriving sexp_of]
    type fun_ [@@deriving sexp_of]
    type tuple [@@deriving sexp_of]
    type type_constructor [@@deriving sexp_of]
    type var [@@deriving sexp_of]
  end
end

module Annotation = struct
  module type S = sig
    module Expression : sig
      type apply [@@deriving sexp_of]
      type const [@@deriving sexp_of]
      type construct [@@deriving sexp_of]
      type constructor [@@deriving sexp_of]
      type lambda [@@deriving sexp_of]
      type let_ [@@deriving sexp_of]
      type match_ [@@deriving sexp_of]
      type record [@@deriving sexp_of]
      type record_field [@@deriving sexp_of]
      type tuple [@@deriving sexp_of]
      type var [@@deriving sexp_of]
    end

    module Pattern : sig
      type construct [@@deriving sexp_of]
      type constructor [@@deriving sexp_of]
      type var [@@deriving sexp_of]
      type tuple [@@deriving sexp_of]
    end

    module Type : Type_annotation.S
  end
end

module Type_var = struct
  module type S = sig
    type t [@@deriving sexp_of]
  end
end

module Type = struct
  module type S = sig
    module Type_var : Type_var.S
    module A : Type_annotation.S

    type t =
      | Var of (Type_var.t, A.var) Annotated.t
      | Apply of
          ((Type_name.t, A.type_constructor) Annotated.t * t list, A.apply) Annotated.t
      | Fun of (t list * t, A.fun_) Annotated.t
      | Tuple of (t list, A.tuple) Annotated.t
    [@@deriving sexp_of, variants]

    module Constraint : sig
      type ty := t

      type t =
        { type_class : Type_class_name.t
        ; arg : ty
        }
      [@@deriving sexp_of]
    end

    module Poly : sig
      type ty := t

      type t =
        { quantifiers : Type_var.t list
        ; body : ty
        ; constraints : Constraint.t list
        }
      [@@deriving sexp_of]
    end

    val fold_free_type_vars : t -> init:'acc -> f:('acc -> Type_var.t -> 'acc) -> 'acc
    val iter_free_type_vars : t -> f:(Type_var.t -> unit) -> unit
    val free_type_vars : t -> Type_var.t list
    val generalize : t -> Poly.t
    val const : (Type_name.t, A.type_constructor) Annotated.t -> annotation:A.apply -> t
  end
end

module type Base_ast = sig
  module Type : sig
    module type S = Type.S

    module Make (Type_var : Type_var.S) (A : Type_annotation.S) :
      S with module Type_var := Type_var and module A := A
  end

  module Make (Type_var : Type_var.S) (A : Annotation.S) : sig
    module Type : Type.S with module Type_var := Type_var and module A := A.Type

    module Const : sig
      type t =
        | Int of string
        | String of string
      [@@deriving sexp_of, variants]

      val intrinsic_type : t -> Intrinsic.Type.t
    end

    module Pattern : sig
      type t =
        | Construct of
            ( (Constructor.t, A.Pattern.constructor) Annotated.t * t option
              , A.Pattern.construct )
              Annotated.t
        | Tuple of (t list, A.Pattern.tuple) Annotated.t
        | Var of (Ident.t, A.Pattern.var) Annotated.t
      [@@deriving sexp_of]
    end

    module Expression : sig
      type t =
        | Var of (Ident.t, A.Expression.var) Annotated.t
        | Apply of (t * t list, A.Expression.apply) Annotated.t
        | Lambda of (Ident.t list * t, A.Expression.lambda) Annotated.t
        | Let of
            { name : Ident.t
            ; value : t
            ; in_ : t
            ; annotation : A.Expression.let_
            }
        | Const of (Const.t, A.Expression.const) Annotated.t
        | Tuple of (t list, A.Expression.tuple) Annotated.t
        | Construct of
            ( (Constructor.t, A.Expression.constructor) Annotated.t * t option
              , A.Expression.construct )
              Annotated.t
        | Record of
            ( ((Field_name.t, A.Expression.record_field) Annotated.t * t) list
              , A.Expression.record )
              Annotated.t
        | Match of
            { scrutinee : t
            ; cases : (Pattern.t * t) list
            ; annotation : A.Expression.match_
            }
      [@@deriving sexp_of]

      val const_int : int -> annot:A.Expression.const -> t
      val const_string : string -> annot:A.Expression.const -> t
      val is_syntactic_value : t -> bool
    end
  end
end
