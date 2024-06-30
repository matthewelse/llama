open! Core
open! Import

module Base = struct
  module Var = struct
    type t = string
  end

  type expression =
    | Literal of int
    | Var of Var.t
    | Ann of expression * type_
    | Abs of Var.t * expression
    | App of expression * expression

  and type_ =
    | Int
    | Fun of type_ * type_
end

module Functor = struct
  module Make (A : sig
      type abs
      type ann
      type app
      type literal
      type var
      type descriptor
    end) =
  struct
    module Var = struct
      type t = string
    end

    type expression =
      | Literal of A.literal * int
      | Var of A.var * Var.t
      | Ann of A.ann * expression * type_
      | Abs of A.abs * Var.t * expression
      | App of A.app * expression * expression
      | Ext of A.descriptor

    and type_ =
      | Int
      | Fun of type_ * type_
  end
end

module Object_phantoms = struct
  module Var = struct
    type t = string
  end

  type 'a expression =
    | Literal of 'literal * int
    | Var of 'var * Var.t
    | Ann of 'ann * 'a expression * type_
    | Abs of 'abs * Var.t * 'a expression
    | App of 'app * 'a expression * 'a expression
    constraint
      'a =
      < literal : 'literal ; var : 'var ; ann : 'ann ; abs : 'abs ; app : 'app >

  and type_ =
    | Int
    | Fun of type_ * type_

  type frontend_phantom =
    < literal : Span.t ; var : Span.t ; ann : Span.t ; abs : Span.t ; app : Span.t >
end
