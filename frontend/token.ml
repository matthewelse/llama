open! Core

type token =
  | Int of string
  | String of string
  | Type_var of string
  | Ident of string
  | Ampersand
  | Array
  | Arrow
  | Assign
  | Break
  | Class
  | Colon
  | Constructor of string
  | Comma
  | Do
  | Dot
  | DoubleSemicolon
  | Else
  | End
  | Equal
  | For
  | Fun
  | Greater
  | GreaterEqual
  | If
  | Impl
  | In
  | Intrinsic
  | Lbrace
  | Lbracket
  | Less
  | LessEqual
  | Let
  | Lparen
  | Match
  | Minus
  | Nil
  | NotEqual
  | Of
  | Pipe
  | Plus
  | Rbrace
  | Rbracket
  | Rparen
  | Semicolon
  | Sig
  | Slash
  | Star
  | Struct
  | Then
  | To
  | Type
  | Val
  | Var
  | Where
  | While
  | With
  | Eof
[@@deriving sexp_of]
