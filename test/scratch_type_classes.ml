open! Core
open! Import

(* Messing with the 1994 paper about type classes in Haskell *)

module Ident =
  String_id.Make
    (struct
      let module_name = "Ident"
    end)
    ()

module Type_var =
  String_id.Make
    (struct
      let module_name = "Type_var"
    end)
    ()

module Type_constructor =
  String_id.Make
    (struct
      let module_name = "Type_constructor"
    end)
    ()

module Type_class_name =
  String_id.Make
    (struct
      let module_name = "Type_class_name"
    end)
    ()

module Simple_type = struct
  type t =
    | Arrow of t * t
    | Var of Type_var.t
    | Construct of Type_constructor.t * t list
  [@@deriving sexp_of]

  let rec pp fmt t =
    match t with
    | Arrow (t1, t2) -> Format.fprintf fmt "(%a → %a)" pp t1 pp t2
    | Var type_var -> Format.pp_print_string fmt (Type_var.to_string type_var)
    | Construct (type_constructor, types) ->
      if List.is_empty types
      then Format.pp_print_string fmt (Type_constructor.to_string type_constructor)
      else
        Format.fprintf
          fmt
          "%a %s"
          (Format.pp_print_list pp)
          types
          (Type_constructor.to_string type_constructor)
  ;;
end

let pp_constraints fmt constraints pp_typ =
  if not (List.is_empty constraints)
  then (
    Format.pp_print_string fmt "⟨ ";
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
      (fun fmt (type_class_name, typ) ->
        Format.fprintf fmt "%s %a" (Type_class_name.to_string type_class_name) pp_typ typ)
      fmt
      constraints;
    Format.pp_print_string fmt " ⟩";
    Format.pp_print_string fmt " => ")
;;

module Overloaded_type = struct
  type t =
    { constraints : (Type_class_name.t * Simple_type.t) list
    (** c.f. [Polymorphic_type.constraints], which only has [Type_var.t] on the right-hand side. *)
    ; type_ : Simple_type.t
    }
  [@@deriving sexp_of]

  let pp { constraints; type_ } fmt =
    pp_constraints fmt constraints Simple_type.pp;
    Simple_type.pp fmt type_
  ;;
end

module Context = struct
  (** c.f. [Overloaded_type.constraints], which has [Simple_type.t] on the right-hand side. *)
  type t = (Type_class_name.t * Type_var.t) list [@@deriving sexp_of]
end

module Polymorphic_type = struct
  type t =
    { vars : Type_var.t list
    ; constraints : Context.t
    ; type_ : Simple_type.t
    }
  [@@deriving sexp_of]

  let pp fmt { vars; constraints; type_ } =
    if not (List.is_empty vars)
    then (
      Format.pp_print_string fmt "∀ ";
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.pp_print_string fmt " ")
        Type_var.pp
        fmt
        vars;
      Format.pp_print_string fmt ". ");
    pp_constraints fmt constraints Type_var.pp;
    Simple_type.pp fmt type_
  ;;
end

let%expect_test "_" =
  (* search :: Ord a => a -> [a] -> bool *)
  let alpha = Type_var.of_string "'a" in
  Polymorphic_type.pp
    Format.std_formatter
    { vars = [ alpha ]
    ; constraints = [ Type_class_name.of_string "Ord", alpha ]
    ; type_ =
        Arrow
          ( Var alpha
          , Arrow
              ( Construct (Type_constructor.of_string "list", [ Var alpha ])
              , Construct (Type_constructor.of_string "bool", []) ) )
    };
  [%expect {| ∀ 'a. ⟨ Ord 'a ⟩ => ('a → ('a list → bool)) |}]
;;

module Source_language = struct
  module Expression = struct
    type t =
      | Var of Ident.t
      | Apply of (t * t)
      | Abstract of Ident.t * t
      | Let of
          { name : Ident.t
          ; value : t
          ; body : t
          }

    let rec pp fmt t =
      match t with
      | Var ident -> Format.pp_print_string fmt (Ident.to_string ident)
      | Apply (e1, e2) -> Format.fprintf fmt "(%a %a)" pp e1 pp e2
      | Abstract (ident, e) ->
        Format.fprintf fmt "(λ %s. %a)" (Ident.to_string ident) pp e
      | Let { name; value; body } ->
        Format.fprintf fmt "(let %s = %a\n in %a)" (Ident.to_string name) pp value pp body
    ;;
  end

  module Class_declaration = struct
    (** {v
        class [context] => [type_class] [type_var] where
          [decls]
        v} *)

    type t =
      { context : Context.t
      ; implements : Type_class_name.t * Type_var.t
      ; decls : (Ident.t * Simple_type.t) list
      }
  end

  module Instance_declaration = struct
    (** {v
        instance [context] => [type_class] ([type_constructor] [type_vars]) where
          [decls]
        v} *)

    type t =
      { context : Context.t
      ; implements : Type_class_name.t * (Type_constructor.t * Type_var.t list)
      ; decls : (Ident.t * Expression.t) list
      }
  end
end

module Nonempty_list = struct
  type 'a t = ( :: ) of 'a * 'a list [@@deriving sexp_of]

  let iter (hd :: tl) ~f =
    f hd;
    List.iter tl ~f:(fun x -> f x)
  ;;

  let pp ?pp_sep pp fmt t : unit =
    Format.pp_print_iter ?pp_sep (fun f x -> iter x ~f) pp fmt t
  ;;
end

module Target_language = struct
  module Expression = struct
    type t =
      | Var of Ident.t
      | Abstract of (Ident.t * Polymorphic_type.t) * t
      | Apply of t * t
      | Let of
          { name : Ident.t
          ; value : t
          ; body : t
          }
      | Dictionary of t list
      | Project of t * int
      | Abstract_type of Type_var.t Nonempty_list.t * t
      | Apply_type of t * Simple_type.t Nonempty_list.t
    [@@deriving sexp_of]

    let rec pp fmt t =
      match t with
      | Var ident -> Format.pp_print_string fmt (Ident.to_string ident)
      | Abstract ((ident, typ), e) ->
        Format.fprintf
          fmt
          "(λ %s : %a. %a)"
          (Ident.to_string ident)
          Polymorphic_type.pp
          typ
          pp
          e
      | Apply (e1, e2) -> Format.fprintf fmt "(%a %a)" pp e1 pp e2
      | Let { name; value; body } ->
        Format.fprintf fmt "(let %s = %a\n in %a)" (Ident.to_string name) pp value pp body
      | Dictionary es -> Format.fprintf fmt "{%a}" (Format.pp_print_list pp) es
      | Project (e, i) -> Format.fprintf fmt "%a.%d" pp e i
      | Abstract_type (vars, e) ->
        Format.fprintf
          fmt
          "(Λ %a. %a)"
          (Nonempty_list.pp
             ~pp_sep:(fun fmt () -> Format.pp_print_string fmt " ")
             Type_var.pp)
          vars
          pp
          e
      | Apply_type (e, types) ->
        Format.fprintf
          fmt
          "(%a %a)"
          pp
          e
          (Nonempty_list.pp
             ~pp_sep:(fun fmt () -> Format.pp_print_string fmt " ")
             Simple_type.pp)
          types
    ;;
  end

  module Bindset = struct
    type t = (Ident.t * Expression.t) list [@@deriving sexp_of]

    let pp fmt t =
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.pp_print_string fmt "\n")
        (fun fmt (ident, e) ->
          Format.fprintf fmt "%s = %a" (Ident.to_string ident) Expression.pp e)
        fmt
        t
    ;;
  end

  module Program = struct
    type t = Letrec of Bindset.t * Expression.t

    let pp fmt (Letrec (bindset, e)) =
      Format.fprintf fmt "letrec %a\nin %a" Bindset.pp bindset Expression.pp e
    ;;
  end

  let%expect_test "output language" =
    let x = Ident.of_string "x" in
    let ys = Ident.of_string "ys" in
    let alpha = Type_var.of_string "'a" in
    let dOrd = Ident.of_string "dOrd" in
    let dOrd' x : Simple_type.t = Construct (Type_constructor.of_string "dOrd", [ x ]) in
    let mono ty : Polymorphic_type.t = { vars = []; constraints = []; type_ = ty } in
    let list x : Simple_type.t = Construct (Type_constructor.of_string "list", [ x ]) in
    let search : Expression.t =
      let body : Expression.t =
        let ys_null : Expression.t =
          Apply (Apply_type (Var (Ident.of_string "null"), [ Var alpha ]), Var ys)
        in
        Apply (Var (Ident.of_string "not"), ys_null)
      in
      (* Lambda alpha *)
      Abstract_type
        ( [ alpha ]
        , (* lambda dOrd : (Ord alpha) *)
          Abstract
            ( (dOrd, mono (dOrd' (Var alpha)))
            , (* lambda x : alpha. lambda y : alpha list *)
              Abstract
                ((x, mono (Var alpha)), Abstract ((ys, mono (list (Var alpha))), body)) )
        )
    in
    Expression.pp Format.std_formatter search;
    [%expect
      {| (Λ 'a. (λ dOrd : 'a dOrd. (λ x : 'a. (λ ys : 'a list. (not ((null 'a) ys)))))) |}]
  ;;
end
