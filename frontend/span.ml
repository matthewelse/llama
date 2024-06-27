open! Core
open! Import

type t = Source_code_position.t * Source_code_position.t
[@@deriving compare, equal, sexp_of]

let dummy = Lexing.dummy_pos, Lexing.dummy_pos
