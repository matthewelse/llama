open! Core
include Base_ast_intf

module Type = struct
  module type S = Type.S

  module Make (Type_var : Type_var.S) (A : Type_annotation.S) = struct
    type t =
      | Var of (Type_var.t, A.var) Annotated.t
      | Apply of
          ((Type_name.t, A.type_constructor) Annotated.t * t list, A.apply) Annotated.t
      | Fun of (t list * t, A.fun_) Annotated.t
      | Tuple of (t list, A.tuple) Annotated.t
    [@@deriving sexp_of, variants]

    module Constraint = struct
      type ty = t [@@deriving sexp_of]

      type t =
        { type_class : Type_class_name.t
        ; arg : ty
        }
      [@@deriving sexp_of]
    end

    module Poly = struct
      type ty = t [@@deriving sexp_of]

      type t =
        { quantifiers : Type_var.t list
        ; body : ty
        ; constraints : Constraint.t list
        }
      [@@deriving sexp_of]
    end

    (* FIXME melse: overloaded types *)

    let rec fold_free_type_vars t ~init:acc ~f =
      match t with
      | Var (v, _) -> f acc v
      | Apply ((_, args), _) ->
        List.fold args ~init:acc ~f:(fun acc next ->
          fold_free_type_vars next ~init:acc ~f)
      | Fun ((args, r), _) ->
        let acc =
          List.fold args ~init:acc ~f:(fun acc next ->
            fold_free_type_vars next ~init:acc ~f)
        in
        fold_free_type_vars r ~init:acc ~f
      | Tuple (ts, _) ->
        List.fold ts ~init:acc ~f:(fun acc next -> fold_free_type_vars next ~init:acc ~f)
    ;;

    let iter_free_type_vars t ~f = fold_free_type_vars t ~init:() ~f:(fun () x -> f x)
    let free_type_vars t = fold_free_type_vars t ~init:[] ~f:(fun acc x -> x :: acc)

    let generalize t : Poly.t =
      let quantifiers = free_type_vars t in
      { body = t; quantifiers; constraints = [] }
    ;;

    let const name ~annotation = Apply ((name, []), annotation)
  end
end

module Expression = struct
  module type S = Expression.S

  module Make
      (Type : sig
         type var [@@deriving sexp_of]
         type t [@@deriving sexp_of]
       end)
      (A : Expression_annotation.S) =
  struct
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

    module Pattern = struct
      type t =
        | Construct of
            ( (Constructor.t, A.Pattern.constructor) Annotated.t * t option
              , A.Pattern.construct )
              Annotated.t
        | Tuple of (t list, A.Pattern.tuple) Annotated.t
        | Var of (Ident.t, A.Pattern.var) Annotated.t
      [@@deriving sexp_of]
    end

    module Expression = struct
      type t =
        | Var of (Ident.t, A.Expression.var) Annotated.t
        | Apply of (t * t list, A.Expression.apply) Annotated.t
        | Lambda of
            ( (Ident.t, A.Expression.lambda_arg) Annotated.t list * t
              , A.Expression.lambda )
              Annotated.t
        | Let of
            { name : (Ident.t, A.Expression.let_name) Annotated.t
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
        | TFun of (Type.var list * t, A.Expression.tfun) Annotated.t
        | TApply of (t * Type.t, A.Expression.tapply) Annotated.t
      [@@deriving sexp_of]

      let const_int n ~annot = Const (Int (Int.to_string n), annot)
      let const_string s ~annot = Const (String s, annot)
    end
  end
end

module Make (Type_var : Type_var.S) (A : Annotation.S) = struct
  module Type = Type.Make (Type_var) (A.Type)

  include
    Expression.Make
      (struct
        type t = Type.t [@@deriving sexp_of]
        type var = Type_var.t [@@deriving sexp_of]
      end)
      (A)
end
