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
%token Class        "class"
%token Colon        ":"
%token Comma        ","
%token Do           "do"
%token Dot          "."
%token Else         "else"
%token End          "end"
%token Equal        "="
%token For          "for"
%token Fun          "fun"
%token Greater      ">"
%token GreaterEqual ">="
%token If           "if"
%token Impl         "impl"
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
%token DoubleSemicolon ";;"
%token Sig          "sig"
%token Slash        "/"
%token Star         "*"
%token Struct       "struct"
%token Then         "then"
%token To           "to"
%token Type         "type"
%token Val          "val"
%token Var          "var"
%token Where        "where"
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
  | ~ = intrinsic_declaration; <Structure_item.Intrinsic>
  | ~ = let_binding;           <Structure_item.Let>
  | ~ = type_class_declaration;<Structure_item.Type_class_declaration>
  | ~ = type_class_implementation; <Structure_item.Type_class_implementation>
  | ~ = type_declaration;      <Structure_item.Type_declaration>

(* Global variable declarations *)

let let_binding ==
  | "let" ; name = located(ident); "="; value = expression; option(";;"); { { Let_binding.name; value; loc = $sloc } }

(* Type declarations *)

let type_vars ==
  | type_var = located(Type_var); { [ type_var ] }
  | "(" ; type_vars = separated_nonempty_list(",", located(Type_var)); ")" ; { type_vars }

let located(A) == | value = A; { ({ value; loc = $sloc } : _ Located.t) }

let type_declaration ==
  | "type"; name = located(type_id); "="; desc = type_desc;
    { { Type_declaration.name
      ; type_params = []
      ; type_shape = desc
      ; loc = $sloc
      }
    }
  | "type"; type_params = type_vars; name = located(type_id); "="; desc = type_desc;
    {
      { Type_declaration.name
      ; type_params
      ; type_shape = desc
      ; loc = $sloc
      }
    }

let type_desc :=
  | "{"; fields = record_fields; "}"; { Type_shape.Record { fields } }
  | constructors = variant;           { Type_shape.Variant { constructors } }

let variant :=
  | option("|"); constructors = separated_list("|", constructor); { constructors }

let constructor :=
  | constructor_name = located(constructor_name); "of"; ~ = type_; { (constructor_name, Some type_) }
  | constructor_name = located(constructor_name); { (constructor_name, None) }

let record_fields == separated_nonempty_list(";", record_field)

let record_field :=
  | field_name = located(field_id); ":"; ~ = type_; { (field_name, type_) }

(* Intrinsic declaration *)

let intrinsic_declaration :=
  | "intrinsic"; name = located(ident); ":"; ~ = type_; "="; intrinsic_name = located(String); {
    { Value_intrinsic.name
    ; intrinsic = Located.map ~f:Intrinsic.Value.of_string intrinsic_name
    ; type_ = Type.generalize type_
    ; loc = $sloc
    }
  }

let base_type :=
  | v = Type_var;    { { Type.desc = Var v; loc = $sloc } }
  | type_id = located(type_id);     { { Type.desc = Apply (type_id, []); loc = $sloc } }
  | type_var = Type_var; type_id = located(type_id);     { { Type.desc = Apply (type_id, [ { desc = Var type_var; loc = $loc(type_var) } ]); loc = $sloc } }
  | "("; ~ = type_; ")"; <>

let inter_type :=
  | ~ = base_type; "*"; ~ = inter_type; {
    match (inter_type : Type.t) with
    | { desc = Tuple elems; _ } -> { inter_type with desc = Tuple { value = base_type :: elems.value; loc = $sloc } }
    | _ -> { Type.desc = Tuple { value = [ base_type; inter_type ]; loc = $sloc }; loc = $sloc }
  }
  | ~ = base_type; { base_type }

let type_ :=
  | ~ = inter_type; "->"; ~ = type_; {
    match (type_ : Type.t) with
    | { desc = Fun (args, ret); _} -> { type_ with desc = Type.Fun (inter_type :: args, ret)}
    | _ -> { Type.desc = Fun([ inter_type ], type_)
           ; loc = $loc
           }
  }
  | ~ = inter_type; { inter_type }

(* Type classes *)

let type_class_declaration :=
  | "class"; name = located(type_class_name); "("; arg = located(Type_var); ")"; constraints = option(where_clause); ":"; "sig"; functions = list(type_class_sig); "end"; {
    { Type_class_declaration.name
    ; arg
    ; functions
    ; constraints = Option.value ~default:[] constraints
    }
  }

let where_clause :=
  | "where"; constraints = separated_list(",", type_constraint); { constraints }

let type_constraint :=
  | type_class = located(type_class_name); "("; arg = located(Type_var); ")"; { { Type_constraint.arg; type_class } }

let type_class_name :=
  | name = Constructor; <Type_class_name.of_string>

let type_class_sig :=
  | "val"; name = located(ident); ":"; ty = type_; { { Type_class_declaration.Function_decl.name; ty } }

let type_class_implementation :=
  | "impl" ; name = located(type_class_name);
    "(";  args = separated_list(",", located(Type_var)); type_name = located(type_id); ")";
    constraints = option(where_clause);
    "="; "struct";
    functions = list(type_class_impl); "end"; {
    { Type_class_implementation.name
    ; for_ = (type_name, args)
    ; functions
    ; constraints = Option.value ~default:[] constraints
    }
  }

let type_class_impl :=
  | "let"; name = located(ident); "="; value = expression; option(";;"); { { Type_class_implementation.Function_impl.name; value } }

(* Expressions *)

let expression :=
  | expr = one_expression; { ({ desc = expr; loc = $sloc } : Expression.t) }
  (* | "("; ~ = separated_list(";", one_expression); ")"; <Sequence> *)

let one_expression :=
  | ~ = literal; { Const literal }
  (* | "-"; ~ = expression; <Negative> *)
  (* | e1 = expression; ~ = binop; e2 = expression; { Binary (binop, e1, e2) } *)
  | "{"; fields = separated_nonempty_list(";", expr_record_field); "}"; { Record fields }
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
  | "let"; name = ident; "="; value = expression; "in"; in_ = expression; { Let { name; value; in_ } }
  | constructor = located(constructor_name); ~ = expression; { Construct (constructor, Some expression) }
  | constructor = located(constructor_name); { Construct (constructor, None) }
  | func = ident; args = located(function_args); { Apply ({ desc = Var func; loc = $loc(func) }, args) }
  | "("; ~ = expression; ")"; { expression.desc }
  | "("; fst = expression; ","; args = separated_nonempty_list(",", expression); ")"; { Tuple (fst :: args) }
  | "match"; scrutinee = expression; "with"; option("|"); cases = separated_nonempty_list("|", match_case); { Match { scrutinee; cases } }
  | "fun"; "(" ; args = separated_list(",", ident); ")"; "->"; body = expression; { Lambda (args, body) }

let function_args ==
  | "("; args = separated_list(",", expression); ")"; { args }

let match_case :=
  | ~ = pattern; "->"; ~ = expression; { (pattern, expression) }

let pattern_desc :=
  | constructor = located(constructor_name); ~ = pattern; { Pattern.Construct (constructor, Some pattern) }
  | constructor = located(constructor_name); { Pattern.Construct (constructor, None) }
  | ~ = located(ident); <Pattern.Var>
  | "("; ~ = pattern_desc; ")"; { pattern_desc }
  | "("; fst = pattern; ","; args = separated_nonempty_list(",", pattern); ")"; { Pattern.Tuple { value = fst :: args; loc = $sloc } }

let pattern :=
  | pattern = pattern_desc; { { Pattern.desc = pattern; loc = $sloc } }

let expr_record_field :=
  | ~ = located(field_id); "="; ~ = expression; <>

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
