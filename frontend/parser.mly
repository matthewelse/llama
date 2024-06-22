%{
open! Core
open! Import

open Ast
open Expression
%}

%token<string> Int    "int"
%token<string> Constructor "constructor"
%token<string> String "string"
%token<string> Type_var "type_var"
%token<string> Ident
%token Ampersand    "&"
%token Array        "array"
%token Arrow        "->"
%token Assign       ":="
%token Break        "break"
%token Colon        ":"
%token Comma        ","
%token Do           "do"
%token Dot          "."
%token Else         "else"
%token End          "end"
%token Equal        "="
%token For          "for"
%token Function     "function"
%token Greater      ">"
%token GreaterEqual ">="
%token If           "if"
%token In           "in"
%token Intrinsic    "intrinsic"
%token Lbrace       "{"
%token Lbracket     "["
%token Less         "<"
%token LessEqual    "<="
%token Let          "let"
%token Lparen       "("
%token Match        "match"
%token Minus        "-"
%token Nil          "nil"
%token NotEqual     "<>"
%token Of           "of"
%token Pipe         "|"
%token Plus         "+"
%token Rbrace       "}"
%token Rbracket     "]"
%token Rparen       ")"
%token Semicolon    ";"
%token Slash        "/"
%token Star         "*"
%token Then         "then"
%token To           "to"
%token Type         "type"
%token Var          "var"
%token While        "while"
%token With         "with"
%token Eof

%nonassoc Else
%nonassoc Of

%nonassoc Assign
%left Pipe, Ampersand
%nonassoc LessEqual, GreaterEqual, Less, Greater, NotEqual, Equal
%left Plus, Minus
%left Star, Slash

%start<Ast.t> program

%%

let program := ~ = list(structure_item); Eof; <>

(* Declarations *)

let structure_item :=
  | ~ = type_declaration;      <Structure_item.Type_declaration>
  | ~ = let_binding;           <Structure_item.Let>
  | ~ = intrinsic_declaration; <Structure_item.Intrinsic>

(* Global variable declarations *)

let let_binding ==
  | "let" ; name = ident; "="; value = expression; { {Let_binding.name; value } }

(* Function declarations *)

(* Type declarations *)

let type_vars ==
  | type_var = Type_var; { [ type_var ] }
  | "(" ; type_vars = separated_nonempty_list(",", Type_var); ")" ; { type_vars }

let type_declaration ==
  | "type"; name = type_id; "="; desc = type_desc;
    { { Type_declaration.name
      ; type_params = []
      ; type_shape = desc
      }
    }
  | "type"; type_params = type_vars; name = type_id; "="; desc = type_desc;
    {
      { Type_declaration.name
      ; type_params
      ; type_shape = desc
      }
    }

let type_desc :=
  | id = type_id;                     { Type_shape.Alias (Apply (id, [])) }
  | "{"; fields = record_fields; "}"; { Type_shape.Record { fields } }
  | name = String;                    { Type_shape.Alias (Intrinsic (Intrinsic.Type.of_string name)) }
  | constructors = variant;           { Type_shape.Variant { constructors } }

let variant :=
  | option("|"); constructors = separated_list("|", constructor); { constructors }

let constructor :=
  | ~ = constructor_name; "of"; ~ = type_; { (constructor_name, Some type_) }
  | ~ = constructor_name; { (constructor_name, None) }

let record_fields == separated_nonempty_list(";", record_field)

let record_field :=
  | field_name = field_id; ":"; ~ = type_; { (field_name, type_) }

let intrinsic_declaration :=
  | "intrinsic"; name = ident; ":"; ~ = type_; "="; intrinsic_name = String; {
    { Value_intrinsic.name; intrinsic = Intrinsic.Value.of_string intrinsic_name; type_ = { quantifiers = []; ty = type_ } }
  }

let base_type :=
  | ~ = Type_var;    <Type.Var>
  | ~ = type_id;     { Type.Apply (type_id, []) }
  | type_var = Type_var; ~ = type_id;     { Type.Apply (type_id, [ Type.Var type_var ]) }
  | "("; ~ = type_; ")"; <>

let inter_type :=
  | ~ = base_type; "*"; ~ = inter_type; {
    match (inter_type : Type.t) with
    | Tuple elems -> Type.Tuple (base_type :: elems)
    | _ -> Type.Tuple([ base_type; inter_type ])
  }
  | ~ = base_type; { base_type }

let type_ :=
  | ~ = inter_type; "->"; ~ = type_; {
    match (type_ : Type.t) with
    | Fun (args, ret) -> Type.Fun (args @ [ inter_type ], ret)
    | _ -> Type.Fun([ inter_type ], type_)
  }
  | ~ = inter_type; { inter_type }

(* Expressions *)

let expression :=
  | ~ = one_expression; <>
  (* | "("; ~ = separated_list(";", one_expression); ")"; <Sequence> *)

let one_expression :=
  | ~ = literal; <Const>
  (* | "-"; ~ = expression; <Negative> *)
  (* | e1 = expression; ~ = binop; e2 = expression; { Binary (binop, e1, e2) } *)
  | "{"; ~ = separated_nonempty_list(",", expr_record_field); "}"; <Record>
  (* We need some redundant indexing rules to work around shift/reduce conflicts. *)
  (* | element_type = type_id; "["; size = expression; "]"; "of"; init = expression;
    { Array { element_type; size; init } } *)
  (* | ~ = ident; "["; size = expression; "]"; ":="; ~ = expression; { Assign (Subscript (Ident ident, size), expression) } *)
  (* | ~ = ident; "["; size = expression; "]"; { Lvalue (Subscript (Ident ident, size)) } *)
  (* Regular lvalue rules. *)
  | ~ = ident; <Var>
  (* | ~ = lvalue; ":="; ~ = expression; <Assign> *)
  (* | "if"; cond = expression; "then"; then_ = expression; "else"; else_ = expression; { If { cond; then_; else_ = Some else_ } }
  | "if"; cond = expression; "then"; then_ = expression; { If { cond; then_; else_ = None } }
  | "while"; cond = expression; "do"; body = expression; { While { cond; body } }
  | "for"; ~ = ident; ":="; lo = expression; "to"; hi = expression; "do"; body = expression; { For { ident; lo; hi; body } }
  | "break"; { Break } *)
  (* | "let"; ~ = declarations; "in"; exps = separated_list(";", expression); "end"; { Let { declarations; exps } } *)
  | constructor = constructor_name; ~ = expression; { Construct (constructor, Some expression) }
  | constructor = constructor_name; { Construct (constructor, None) }
  | func = ident; "("; args = separated_list(",", expression); ")"; { Apply (Var func, args) }
  | "("; ~ = expression; ")"; { expression }
  | "("; fst = expression; ","; args = separated_nonempty_list(",", expression); ")"; { Tuple (fst :: args) }
  | "match"; scrutinee = expression; "with"; option("|"); cases = separated_nonempty_list("|", match_case); { Match { scrutinee; cases } }

let match_case :=
  | ~ = pattern; "->"; ~ = expression; { (pattern, expression) }

let pattern :=
  | constructor = constructor_name; ~ = pattern; { Pattern.Construct (constructor, Some pattern) }
  | constructor = constructor_name; { Pattern.Construct (constructor, None) }
  | ~ = ident; { Pattern.Var ident }
  | "("; ~ = pattern; ")"; { pattern }
  | "("; fst = pattern; ","; args = separated_nonempty_list(",", pattern); ")"; { Pattern.Tuple (fst :: args) }

let expr_record_field :=
  | ~ = field_id; "="; ~ = expression; <>

(* let binop ==
  | "|";  { Binary_operator.Or }
  | "&";  { Binary_operator.And }
  | "<="; { Binary_operator.Le }
  | ">="; { Binary_operator.Ge }
  | "<";  { Binary_operator.Lt }
  | ">";  { Binary_operator.Gt }
  | "<>"; { Binary_operator.NotEqual }
  | "=";  { Binary_operator.Equal }
  | "-";  { Binary_operator.Minus }
  | "+";  { Binary_operator.Plus }
  | "/";  { Binary_operator.Divide }
  | "*";  { Binary_operator.Times }
*)

let literal ==
  | ~ = "int"   ; <Const.Int>
  | ~ = "string"; <Const.String>

(* Lvalues *)

(*
let lvalue :=
  | ~ = ident; <Ident>
  | ~ = lvalue; "."; ~ = field_id; <Dot>
  | ~ = lvalue; "["; ~ = expression; "]"; <Subscript>
  *)

(* Identifiers *)

let type_id ==
  | ~ = Ident; <Type_name.of_string>

let field_id ==
  | ~ = Ident; <Field_name.of_string>

let ident ==
  | ~ = Ident; <Ident.of_string>

let constructor_name ==
  | ~ = Constructor; <Constructor.of_string>
