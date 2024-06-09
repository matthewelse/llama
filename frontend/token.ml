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
  | Colon
  | Constructor of string
  | Comma
  | Do
  | Dot
  | Else
  | End
  | Equal
  | For
  | Function
  | Greater
  | GreaterEqual
  | If
  | In
  | Intrinsic
  | Lbrace
  | Lbracket
  | Less
  | LessEqual
  | Let
  | Lparen
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
  | Slash
  | Star
  | Then
  | To
  | Type
  | Var
  | While
  | Eof
[@@deriving sexp_of]
