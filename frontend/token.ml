open! Core

type token =
  | Int of string
  | String of string
  | Type_var of string
  | Ident of string
  | Ampersand
  | Array
  | Assign
  | Break
  | Colon
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
