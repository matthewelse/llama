%{
open! Core
open! Import

module Expression = Ast.Expression
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

let located(A) == | value = A; { (value, $sloc) }

let program :=
  | ~ = list(structure_item); Eof; <>

let structure_item :=
  | ~ = intrinsic_declaration;     <Intrinsic>
  | ~ = let_binding;               <Let>
  | ~ = type_class_declaration;    <Type_class_declaration>
  | ~ = type_class_implementation; <Type_class_implementation>
  | ~ = type_declaration;          <Ast.Structure_item.Type_declaration>

(* Global variable declarations *)

let let_binding ==
  | "let" ; name = located(ident); "="; value = expression; option(";;"); {
    ({ name; value; loc = $sloc } : Ast.Let_binding.t)
  }

(* Type declarations *)

let type_declaration ==
  | "type"; type_params = option(type_vars); name = located(type_id); "="; ~ = type_shape;
    { ({ name
       ; type_params = Option.value type_params ~default:[]
       ; type_shape
       ; loc = $sloc
       } : Ast.Type_declaration.t)
    }

let type_vars ==
  |       type_var  =                              located("type_var");       { [ type_var ] }
  | "(" ; type_vars = separated_nonempty_list(",", located("type_var")); ")"; { type_vars }

(* A type shape defines the "shape" of a new variant type or record type. *)

let type_shape :=
  | fields = record_declaration;             { (Record  { fields }       : Ast.Type_shape.t) }
  | constructors = constructor_declarations; { (Variant { constructors } : Ast.Type_shape.t) }

let constructor_declarations :=
  | option("|"); constructors = separated_list("|", constructor_declaration); { constructors }

let constructor_declaration :=
  | constructor_name = located(constructor_name); "of"; ~ = type_; { (constructor_name, Some type_) }
  | constructor_name = located(constructor_name); { (constructor_name, None) }

let record_declaration ==
  | "{"; fields = separated_nonempty_list(";", record_field_declaration); "}"; { fields }

let record_field_declaration :=
  | field_name = located(field_id); ":"; ~ = type_; { (field_name, type_) }

(* Intrinsic declaration *)

let intrinsic_declaration :=
  | "intrinsic"; name = located(ident); ":"; ~ = type_; "="; intrinsic_name = located(String); {
    let (intrinsic_name, intrinsic_loc) = intrinsic_name in
    { Ast.Value_intrinsic.name
    ; intrinsic = (Intrinsic.Value.of_string intrinsic_name, intrinsic_loc)
    ; type_ = Ast.Type.generalize type_
    ; loc = $sloc
    }
  }

(* Types - used for annotating expressions, for fields of records, intrinsics etc. *)

let base_type :=
  | v = "type_var";             { (Var (v, $sloc) : Ast.Type.t) }
  | type_id = located(type_id); { (Apply ((type_id, []), $sloc) : Ast.Type.t) }
  | "("; ~ = type_; ")";        { type_ }

let type_ :=
  | "("; elems = separated_nonempty_list(",", type_); ")";                             { (Tuple (elems, $sloc) : Ast.Type.t) }

  | "("; ")" ; "->"; return_type = type_;                                              { (Fun (([]   , return_type), $sloc) : Ast.Type.t) }
  |      arg = base_type;  "->"; return_type = type_;                                  { (Fun (([arg], return_type), $sloc) : Ast.Type.t) }
  | "(";  args = separated_nonempty_list(",", type_); ")"; "->"; return_type = type_;  { (Fun (( args, return_type), $sloc) : Ast.Type.t) }

  | "(";  args = separated_nonempty_list(",", type_); ")"; type_id = located(type_id); { (Apply ((type_id, args), $sloc) : Ast.Type.t) }
  | ~ = base_type; type_id = located(type_id);                                         { (Apply ((type_id, [ base_type ]), $sloc) : Ast.Type.t) }

  | ~ = base_type;                                                                     { base_type }

(* Type classes *)

let type_class_declaration :=
  | "class"; name = located(type_class_name); "("; arg = located(Type_var); ")"; constraints = option(where_clause); ":"; "sig"; functions = list(type_class_sig); "end"; {
    { name
    ; arg
    ; functions
    ; constraints = Option.value ~default:[] constraints
    }
  }

let where_clause :=
  | "where"; constraints = separated_list(",", type_constraint); { constraints }

let type_constraint :=
  | type_class = located(type_class_name); "("; arg = located(Type_var); ")"; { { Ast.Type_constraint.arg; type_class } }

let type_class_name :=
  | name = Constructor; <Type_class_name.of_string>

let type_class_sig :=
  | "val"; name = located(ident); ":"; ty = type_; { ({ name; ty }: Ast.Type_class_declaration.Function_decl.t) }

let type_class_implementation :=
  | "impl" ; name = located(type_class_name);
    "(";  args = separated_list(",", located(Type_var)); type_name = located(type_id); ")";
    constraints = option(where_clause);
    "="; "struct";
    functions = list(type_class_impl); "end"; {
    { Ast.Type_class_implementation.name
    ; for_ = (type_name, args)
    ; functions
    ; constraints = Option.value ~default:[] constraints
    }
  }

let type_class_impl :=
  | "let"; name = located(ident); "="; value = expression; option(";;"); { { Ast.Type_class_implementation.Function_impl.name; value } }

(* Expressions *)

let expression :=
  | expr = one_expression; { expr }
  (* | "("; ~ = separated_list(";", one_expression); ")"; <Sequence> *)

let one_expression :=
  | ~ = literal; { Const (literal, $sloc) }
  (* | "-"; ~ = expression; <Negative> *)
  (* | e1 = expression; ~ = binop; e2 = expression; { Binary (binop, e1, e2) } *)
  | "{"; fields = separated_nonempty_list(";", expr_record_field); "}"; { Record (fields, $sloc) }
  (* We need some redundant indexing rules to work around shift/reduce conflicts. *)
  (* | element_type = type_id; "["; size = expression; "]"; "of"; init = expression;
    { Array { element_type; size; init } } *)
  (* | ~ = ident; "["; size = expression; "]"; ":="; ~ = expression; { Assign (Subscript (Ident ident, size), expression) } *)
  (* | ~ = ident; "["; size = expression; "]"; { Lvalue (Subscript (Ident ident, size)) } *)
  (* Regular lvalue rules. *)
  | ~ = ident; { Var(ident, $sloc) }
  (* | ~ = lvalue; ":="; ~ = expression; <Assign> *)
  (* | "if"; cond = expression; "then"; then_ = expression; "else"; else_ = expression; { If { cond; then_; else_ = Some else_ } }
  | "if"; cond = expression; "then"; then_ = expression; { If { cond; then_; else_ = None } }
  | "while"; cond = expression; "do"; body = expression; { While { cond; body } }
  | "for"; ~ = ident; ":="; lo = expression; "to"; hi = expression; "do"; body = expression; { For { ident; lo; hi; body } }
  | "break"; { Break } *)
  | "let"; name = located(ident); "="; value = expression; "in"; in_ = expression; { Let { name; value; in_; annotation = $sloc } }
  | constructor = constructor_name; ~ = expression; { Construct (((constructor, $loc(constructor)), Some expression), $sloc) }
  | constructor = constructor_name; { Construct (((constructor , $loc(constructor)), None), $sloc) }
  | func = ident; args = function_args; { Apply ((Var (func, $loc(func)), args), $sloc) }
  | "("; ~ = expression; ")"; { expression }
  | "("; ")"; { Tuple ([], $sloc) }
  | "("; fst = expression; ","; args = separated_nonempty_list(",", expression); ")"; { Tuple (fst :: args, $sloc) }
  | "match"; scrutinee = expression; "with"; option("|"); cases = separated_nonempty_list("|", match_case); { Match { scrutinee; cases; annotation = $sloc } }
  | "fun"; "(" ; args = separated_list(",", located(ident)); ")"; "->"; body = expression; { Ast.Expression.Lambda ((args, body), $sloc) }

let function_args ==
  | "("; args = separated_list(",", expression); ")"; { args }

let match_case :=
  | ~ = pattern; "->"; ~ = expression; { (pattern, expression) }

let pattern :=
  | constructor = located(constructor_name); ~ = pattern; { Construct ((constructor, Some pattern), $sloc) }
  | constructor = located(constructor_name); { Construct ((constructor, None), $sloc) }
  | ~ = ident; { Var(ident, $sloc) }
  | "("; ~ = pattern; ")"; { pattern }
  | "("; fst = pattern; ","; args = separated_nonempty_list(",", pattern); ")"; { Ast.Pattern.Tuple (fst :: args, $sloc) }

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
  | ~ = "int"   ; <Ast.Const.Int>
  | ~ = "string"; <Ast.Const.String>

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
