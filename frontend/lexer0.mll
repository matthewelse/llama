{
  open Token
}

let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?

(* Don't attempt to lex negative integers. *)
let int = digit digit*

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let atom = [ '0'-'9' '_' ]+
let string = "\"" ['a'-'z' 'A'-'Z' '0'-'9' '_' ' ' '\\' '.' '-' '%']* "\""
let type_var = "'" ['a'-'z'] ['a'-'z' '0'-'9' '_']*

rule read =
  parse
  | white    { read lexbuf }
  | newline  { Lexing.new_line lexbuf; read lexbuf }
  (* | '!'      { Bang } *)
  | ":="     { Assign }
  | ':'      { Colon }
  | ';'      { Semicolon }
  | '.'      { Dot }
  | '-'      { Minus }
  | '+'      { Plus }
  | '/'      { Slash }
  | '*'      { Star }
  | '&'      { Ampersand }
  | '|'      { Pipe }
  | '='      { Equal }
  | "<>"     { NotEqual }
  | "<="     { LessEqual }
  | ">="     { GreaterEqual }
  | '>'      { Greater }
  | '<'      { Less }
  | '('      { Lparen }
  | ')'      { Rparen }
  | '['      { Lbracket }
  | ']'      { Rbracket }
  | '{'      { Lbrace }
  | '}'      { Rbrace }
  | ','      { Comma }
  | "let"    { Let }
  | "in"     { In }
  | "if"     { If }
  | "then"   { Then }
  | "else"   { Else }
  | "type"   { Type }
  | "array"  { Array }
  | "of"     { Of }
  | "var"    { Var }
  | "nil"    { Nil }
  | "function" { Function }
  | "for"    { For }
  | "to"     { To }
  | "while"  { While }
  | "do"     { Do }
  | "break"  { Break }
  | "end"    { End }
  | id       { Ident (Lexing.lexeme lexbuf)}
  | int      { Int (Lexing.lexeme lexbuf) }
  | type_var { Type_var (Lexing.lexeme lexbuf) }
  | string   { String (
    Lexing.sub_lexeme lexbuf (Lexing.lexeme_start lexbuf + 1) (Lexing.lexeme_end lexbuf - 1)) }
  | _        { failwith (("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof      { Eof }
