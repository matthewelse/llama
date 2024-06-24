open! Core
open! Import

type 'a t =
  { value : 'a
  ; loc : Span.t
  }
[@@deriving compare, sexp_of]

let dummy x = { value = x; loc = Lexing.dummy_pos, Lexing.dummy_pos }
let value_equal { value = v1; _ } { value = v2; _ } ~f = f v1 v2
